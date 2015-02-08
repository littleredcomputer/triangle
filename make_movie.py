#!/usr/local/bin/python

import os
import shutil
import subprocess
import itertools
import glob
import re
from   optparse import OptionParser
from   multiprocessing.pool import ThreadPool

soundtrack = 'tri_strack.wav'
movie = 'movie.mp4'
cue_re = re.compile(' */\* (CUE.*) \*/')

def prepare_directory(options):
  # clean movie assembly directory
  if not options.phase: shutil.rmtree(options.movie_dir, ignore_errors=True)
  if not os.path.isdir(options.movie_dir): os.mkdir(options.movie_dir)
  for d, link in [('../../cs', 'tri_strack.csd'),
                  ('../../cs', 'sounds'),
                  ('..',       'texture')]:
    target = os.path.join(options.movie_dir, link)
    if not os.path.islink(target):
      os.symlink(os.path.join(d, link), target)
  os.chdir(options.movie_dir)

def generate_frames(options):
  subprocess.call(['../triangle_{}'.format(options.backend),
                   '-q', 'cue', '-n', '-w'])
  if options.backend == 'pr':
    # harvest CUE directives, since they don't get written to their own
    # file by the POV backend
    with open('cue', 'w') as cuefile:
      for f in glob.iglob('*.pov'):
        try:
          for l in subprocess.check_output(
              ['grep','-h','/\* CUE.* \*/',f]).split('\n'):
            m = cue_re.match(l)
            if m: print >> cuefile, m.group(1)
        except subprocess.CalledProcessError: pass

def render_frames(options):
  pool = ThreadPool(8)
  def batch_convert(renderer, imageglob):
    imagefiles = glob.glob(imageglob)
    imagefiles.sort()
    pngfiles = ('{:06}.png'.format(i) for i in itertools.count())
    if any(pool.map(renderer, itertools.izip(imagefiles, pngfiles))):
      raise Exception('failed to convert all images')

  if options.backend == 'gl':
    converter = os.getenv('MAGICK_HOME') + '/bin/convert'
    def render_image(names):
      result = subprocess.call([converter, '-verbose', names[0], names[1]])
      os.remove(names[0])
      return result
    batch_convert(render_image, '*.p6')
  elif options.backend == 'pr':
    def render_image(names):
      return subprocess.call(['povray', '-D', '-P', '-W1280', '-H720',
                              '+A', '+I' + names[0], '+O' + names[1]])
    batch_convert(render_image, '*.pov')

def render_soundtrack(options):
  subprocess.check_call(['../../cs/make_triangle_strack.py', 'cue',
                         'triangle-ftable', 'triangle-score'])
  subprocess.check_call(['csound', '-d', 'tri_strack.csd', '-o', soundtrack])

def master(options):
  subprocess.check_call(['rm', '-f', movie])
  subprocess.check_call(['ffmpeg', '-shortest', '-i', soundtrack, '-r', '29.98',
                         '-f', 'image2', '-i', '%06d.png', '-b:v', '8192k',
                         '-acodec', 'aac', '-strict', 'experimental',
                         '-b:a', '192k', '-vf', 'vflip',  movie])

def make_movie(options):
  prepare_directory(options)
  phases = [('g', generate_frames),
            ('r', render_frames),
            ('s', render_soundtrack),
            ('m', master)]
  for p, f in phases:
    if not options.phase or p in options.phase:
      f(options)

if __name__ == '__main__':
  parser = OptionParser()
  parser.add_option('-b', '--backend', dest='backend', default='gl')
  parser.add_option('-p', '--phase', dest='phase', default='')
  options, args = parser.parse_args()
  options.movie_dir = 'tmp-{}-movie'.format(options.backend)

  make_movie(options)

