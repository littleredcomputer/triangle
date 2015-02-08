<CsoundSynthesizer>
<CsInstruments>
;
; setup
;
sr          = 44100
kr          = 4410
ksmps       = 10
nchnls      = 1

ga_echo     init 0
ga_reverb   init 0
gk_lfo      init 0
;
; function tables
;
gi_ftres    init 32768
gi_area     ftgen 0, 0, 16384,    -23, "triangle-ftable"
gi_sine     ftgen 0, 0, gi_ftres, 10,  1
gi_cosine   ftgen 0, 0, gi_ftres, 11,  1
gi_saw      ftgen 0, 0, gi_ftres, 10,  1, .5, .3333, .25, .2, .1667, .1429,  \
				  	 			  	   .125, .1111, .1, .0909, .0833, .0769, \
									   .0714, .0667, .0625
gi_square   ftgen 0, 0, gi_ftres, 10,  1, 0, .3333, 0, .2, 0, .1429, 0,      \
				  	 			  	   .1111, 0, .0909, 0, .0769, 0, 0.0666
gi_clarinet ftgen 0, 0, gi_ftres, 10,  1, .04, .99, .12, .53, .11, .26, .05, \
				  	 			  	   .24, .07, .02, .03, .02, .03
gi_triangle ftgen 0, 0, gi_ftres, 10,  1, 0, -.1111, 0, .04, 0, -.0204, 0,   \
				  	 			  	   .0156
gi_impulse  ftgen 0, 0, 256,      1,   "sounds/marmstk1.wav", 0, 0, 0
;
; instruments
;
instr 1
  idur   = p3
  gk_lfo poscil 1, 1.0/idur, gi_area
endin

instr 101
  idur       = p3
  iamp       = p4
  ifreq      = cpspch(p5)
  iindex     = 7
  icrossfade = 0.2
  ivdepth    = 0.1
  ivrate     = 3.3
  ifn        = gi_sine
  a1         fmbell iamp, 2*ifreq, iindex, icrossfade, ivdepth, ivrate, \
  	                ifn, ifn, ifn, ifn, gi_cosine
  ga_echo    = ga_echo + a1
             out a1
endin

instr 102
  idur   = p3
  iamp   = p4
  ifreq  = cpspch(p5)
  ihrd   = 0.5
  ipos   = 0.561
  ivibf  = 6
  ivamp  = 0.05
  ivibfn = gi_sine
  idec   = .1
  a1     vibes iamp, ifreq, ihrd, ipos, gi_impulse, ivibf, \
               ivamp, gi_sine, idec
         out a1
endin

instr 103
  idur  = p3
  iamp  = p4
  ifreq = cpspch(p5)
  inumh = 30
  ilowh = 1
  kmul  line 0, idur, 1
  kenv  linen iamp, 0.2 * idur, idur, 0.3 * idur
  a1    gbuzz kenv, ifreq, inumh, ilowh, kmul, gi_cosine
  out   a1
endin

instr 110
  idur  = p3
  iamp  = p4
  ifreq = cpspch(p5)
  kenv  linen 1, 4, idur, 4
  a1    oscil iamp, ifreq, gi_saw
  a2    oscil iamp/2, ifreq*1.001, gi_saw
  a3    oscil iamp/2, ifreq*0.999, gi_saw
  ap    = (a1+a2+a3)/3
        out kenv * (1-gk_lfo) * ap

endin

instr 111
  idur  = p3
  iamp  = p4
  ifreq = cpspch(p5)
  kenv  linen 1, 4, idur, 4
  a1    oscil iamp, ifreq, gi_square
  a2    oscil iamp/2, ifreq*1.001, gi_square
  a3    oscil iamp/2, ifreq*0.999, gi_square
  ap    = (a1+a2+a3)/3
        out gk_lfo * kenv * ap
endin

instr 200
    iamp  = p4
  start:
          timout 0, .5, continue
          reinit start
  continue:
    kgate linseg 0, 0.01, 1, 0.10, 1, 0.01, 0
    klfo  lpshold 4, 0, 0, gk_lfo
    kfreq = cpsoct(8+klfo)
    a1    oscil iamp*kgate, kfreq, gi_saw
          out a1
endin

instr 201
    ifreq = cpsoct(8+i(gk_lfo))
    a1    oscil 5000, ifreq, gi_saw
          out a1
endin

instr 998
  a1 reverb ga_reverb, 0.1
     out a1
     clear ga_reverb
endin

instr 999
  ;a1 reverb ga_echo, 0.5
  a1 comb ga_echo, 8, .75
     out 0.5*a1
     clear ga_echo
endin

</CsInstruments>
<CsScore>
;
; score voice selections
;
#define Voice1   #i 101#
#define Voice2   #i 101#
#define Voice3   #i 102#
#define Voice4   #i 103#
#define TVoice0  #i 110#
#define TVoice1  #i 111#
#define TAmp     #2000#
#define BAmp     #5000#

#include "triangle-score"
;
; LFO and effect units
;
i1 $DroneStart. $DroneLength.   ; triangle area driven LFO
;i998 0 $TotalDuration.   ; effects unit
;i999 0 $TotalDuration.   ; effects unit

</CsScore>
