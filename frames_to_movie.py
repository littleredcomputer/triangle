#!/usr/bin/python
import glob
import sys
import QTKit
from Foundation import NSNumber
from AppKit import NSImage

time = QTKit.QTMakeTime(20, 600) # QT close to 29.98 fps
attrs = {
  QTKit.QTAddImageCodecType:    "png",
  QTKit.QTAddImageCodecQuality: NSNumber.numberWithLong_(QTKit.codecHighQuality)
}

def create_movie(name):
  print "creating ", name
  movie, err = QTKit.QTMovie.alloc().initToWritableFile_error_(name, None)
  if movie is None:
    errmsg = "Could not create movie file: %s" % (name)
    raise IOError, errmsg
  return movie

def add_frame(movie, imagefile):
  print "adding frame ", imagefile
  img = NSImage.alloc().initWithContentsOfFile_(imagefile)
  movie.addImage_forDuration_withAttributes_(img, time, attrs)

movie = create_movie('/tmp/movie.mov')

for a in sys.argv[1:]:
  for f in glob.iglob(a):
    add_frame(movie, f)

movie.updateMovieFile()

