;; 2D representation of Climate Change Consensus over time
;; Creator: Toby D. Pilditch
;; As usual, all mistakes are my own.

;; Project in collaboration with Stephan Lewandowsky & Jens Madsen

;;===================================================
;; VARIABLE DEFINITIONS
;;===================================================
extensions [csv]

globals[
  csv
  ColdData ;; storage for colddata csv
  HotData ;; storage for HotData csv
  yearCount ;; tick oriented sampling counter
  ColdPlotter ;; for use in plotting what agents are seeing
  HotPlotter ;; for use in plotting what agents are seeing

  ;; Reporters:
  g-ph-sci ; global reporter for neutral scientists (mean p-h)
  g-ph-den ; global reporter for skeptic scientists (mean p-h)
  g-ph-pop ; global reporter for skeptic scientists (mean p-h)
  v-ph-sci ; global reporter for neutral scientists (mean p-h)
  v-ph-den ; global reporter for skeptic scientists (mean p-h)
  v-ph-pop ; global reporter for skeptic scientists (mean p-h)
  ]


breed [SciMems SciMem] ;; (Scientific) community member
breed [ComMems ComMem] ;; (General) community member


turtles-own[

  p-CC ;; current belief in climate change
  todays-data ;; placeholder for current data
  todays-LR ;; current (transmissable) assessment of data
  todays-Slope ;; current reg slope (also transmissable between scientists)
  data-mem ;; agents memory list (forms distribution)
  data-mem-mean ;; mean for agent memory distribution
  data-mem-sd ;; sd for agent memory distribution
  mem-cap ;; Agent memory cap (shorter / longer)
  LR-scale-val ;; scaling shift for LR scale (skewed / faithful)
  data-set-pref ;; cold / hot dataset choice
  skept-class ;; classifier for scientist as either skeptic (0) or not (1)

  ;; communication parameters:
  communicator-YN ; has the agent been selected to communicate their LR? (1 = yes, 0 = no)
  received-YN ; 0 = has not yet recieved a communication this "round"; 1 = has. ;; These get reset every round?
  received-LR ; specifically for the recieved version of an LR (distinct from "todays-LR", which is based on data)
  received-Slope ; for communicating between scientists... (passing slope datapoints)
  ]

patches-own[
  ]

;;===================================================
;; FUNCTIONAL PROCEDURES
;;===================================================

to go

  if yearCount >= length HotData ;; end of sim (post-vote)
    [ stop ]

  ;; Scientists sample and update from data (every X ticks?)
  ;; Note: If it isn't a data tick, the else will try to run a communication tick
  ifelse (ticks mod ticks-to-Data-Sample) = 0
  [ ;; DATA SAMPLING
    ask SciMems
    [data-observe
     ;; Scientists update from data
     update-from-data]
  let hotyearData item yearCount HotData ;;reading off year row
  let hotyearTemp item 1 hotyearData
  set HotPlotter lput hotyearTemp HotPlotter
  let coldyearData item yearCount ColdData ;;reading off year row
  let coldyearTemp item 1 coldyearData
  set ColdPlotter lput coldyearTemp ColdPlotter
  set yearCount (yearCount + 1) ;; ready for next "year" data sample
  ]
  [ ;; COMMUNICATION SAMPLING:
  ;; Scientists communicate to Community (or each other)
  if (ticks mod comFreq) = 0
  [
  ; Select communicators:
  select-communicators
  ; facilitate communication procedure:
  communicate-consensus
  ; Anyone who has received an LR via communication (scientist or general public) this tick, will update:
  ask ComMems with [received-YN = 1]
    [update-from-LR]
  ask SciMems with [received-YN = 1]
    [update-from-Slope]
  ]
  ]
  ;; reseting communicator and reciever tags:
  ask SciMems [set communicator-YN 0
               set received-YN 0]
  ask ComMems [set received-YN 0]

  ; color change for visualisation
  ask turtles[
  if p-CC < .1
    [set color blue]
  if (p-CC >= .1) and (p-CC < .2)
    [set color sky]
  if (p-CC >= .2) and (p-CC < .3)
    [set color cyan]
  if (p-CC >= .3) and (p-CC < .4)
    [set color turquoise]
  if (p-CC >= .4) and (p-CC < .5)
    [set color lime]
  if (p-CC >= .5) and (p-CC < .6)
    [set color green]
  if (p-CC >= .6) and (p-CC < .7)
    [set color yellow]
  if (p-CC >= .7) and (p-CC < .8)
    [set color brown]
  if (p-CC >= .8) and (p-CC < .9)
    [set color orange]
  if p-CC >= .9
    [set color red]
  ]

  if skepticProp < 100
  [set g-ph-sci mean [p-CC] of SciMems with [skept-class = 1]; global reporter for neutral scientists (mean p-h)
  set v-ph-sci variance [p-CC] of SciMems with [skept-class = 1]]; global reporter for neutral scientists (var p-h)
  if skepticProp > 0
  [set g-ph-den mean [p-CC] of SciMems with [skept-class = 0]; global reporter for skeptic scientists (mean p-h)
  set v-ph-den variance [p-CC] of SciMems with [skept-class = 0]]; global reporter for skeptic scientists (var p-h)
  set g-ph-pop mean [p-CC] of ComMems; global reporter for skeptic scientists (mean p-h)
  if numComMembers > 1
  [set v-ph-pop variance [p-CC] of ComMems]; global reporter for skeptic scientists (var p-h)

  tick
end

;; AGENTS SAMPLE FROM DATASETS FUNCTION
to data-observe
    ; Draw sample from data:
    if data-set-pref = 1 ;; hot data set preference
      [let hotyearData item yearCount HotData ;;reading off year row
       let hotyearTemp item 1 hotyearData ;;reading off temp for that year
       set data-mem lput hotyearTemp data-mem;; adding the temperature point to agent memory
       set todays-data hotyearTemp ; Store data-point as current evidence
      ]
      if data-set-pref = 0 ;; cold data set preference
      [let coldyearData item yearCount ColdData ;;reading off year row
       let coldyearTemp item 1 coldyearData ;;reading off temp for that year
       set data-mem lput coldyearTemp data-mem;; adding the temperature point to agent memory
       set todays-data coldyearTemp ; Store data-point as current evidence
      ]
    ; Ensuring agent memory reamins within set bounds:
    if length data-mem > mem-cap
    [set data-mem remove-item 0 data-mem] ; delete earliest memory from beginning
    ; update agent distribution here:
    set data-mem-mean mean data-mem;; mean for agent memory distribution
    set data-mem-sd standard-deviation data-mem;; sd for agent memory distribution
end

;; UPDATE FUNCTIONS

; From Data:
to update-from-data
  ;; The basic premise would be based around (10 ^ X) / 1, where X is temp trend.
  ; In this way, LRs are >1 in increasing amounts for pos, and <1 in increasing amounts for neg.
  ; To begin with, these could simply be shifted across one (so that 0 is taken to be evidence AGAINST (LR < 1)) for skeptics.
  let LR-Base 10 ;;2 is standard
  ; Check where evidence has come from (this may determine things...)

  ;; Calculating the agent's regression slope based on their memory window. This slope is the evidence put through LR scaling.
  ; creating variables for regression equation
  let regcount 0
  let Templist [] ;Y
  let Yearlist [] ;X
  let TempDevlist [] ; (Y - Mean(Y))
  let YearDevlist [] ; (X - Mean(X))
  let TempYearDevProductlist [] ; (X - Mean(X))  * (Y - Mean(Y))
  let YearDevSqrdlist []; (X - Mean(X)) ^ 2
  while [regcount < length data-mem]
  [
    set Templist lput item regcount data-mem Templist
    set Yearlist lput (regcount + 1) Yearlist
    set TempDevlist lput ((item regcount data-mem) - data-mem-mean) TempDevList
    set YearDevlist lput ((regcount + 1) - (((length data-mem) + 1) / 2)) YearDevList
    set TempYearDevProductlist lput ((item regcount YearDevlist) * (item regcount TempDevlist)) TempYearDevProductlist
    set YearDevSqrdlist lput ((item regcount YearDevlist) ^ 2) YearDevSqrdlist

    set regcount (regcount + 1)
  ]
  let regSlope ((sum TempYearDevProductlist) / (sum YearDevSqrdlist)) ; using the above to calculate reg slope
  let regIntercept (data-mem-mean - (regSlope * mean Yearlist))
  ; setting reg slope as updated data for belief update...
  set todays-Slope regSlope

  ; Use LR scale and update P(H):
  set todays-LR ((LR-Base ^ (todays-Slope - LR-scale-val)) / 1)
  let temp-O-CC (p-CC / (1 - p-CC)) ;generating the ODDS form for the prior (necessary for multiplying by LD ro generate posterior odds)
  let new-O-CC (temp-O-CC * todays-LR) ;; BAYESIAN ODDS CALC
  set p-CC (new-O-CC / (1 + new-O-CC)) ;converting back to a posterior PROBABILITY
  ;; Bounds for P(CC):
  if p-CC > .999999
    [set p-CC .999999]
  if p-CC < .000001
    [set p-CC .000001]
end

; From communication
to update-from-LR
  ; update based on recieved LR(?)
  let my-LR received-LR
  let temp-O-CC (p-CC / (1 - p-CC)) ;generating the ODDS form for the prior (necessary for multiplying by LD ro generate posterior odds)
  let new-O-CC (temp-O-CC * my-LR) ;; BAYESIAN ODDS CALC
  set p-CC (new-O-CC / (1 + new-O-CC)) ;converting back to a posterior PROBABILITY
  ;; Bounds for P(CC):
  if p-CC > 0.999999
    [set p-CC 0.999999]
  if p-CC < 0.000001
    [set p-CC 0.000001]
end

to update-from-Slope
  ; update based on recieved slope...
  let LR-Base 10 ;;2 is standard
  ; Use LR scale and update P(H):
  let my-LR ((LR-Base ^ (received-Slope - LR-scale-val)) / 1)
  let temp-O-CC (p-CC / (1 - p-CC)) ;generating the ODDS form for the prior (necessary for multiplying by LD ro generate posterior odds)
  let new-O-CC (temp-O-CC * my-LR) ;; BAYESIAN ODDS CALC
  set p-CC (new-O-CC / (1 + new-O-CC)) ;converting back to a posterior PROBABILITY
  ;; Bounds for P(CC):
  if p-CC > .999999
    [set p-CC .999999]
  if p-CC < .000001
    [set p-CC .000001]

end
;; COMMUNICATE FUNCTION
to select-communicators
  ; selecting the scientists who will be in the communicating "pool" (this could be a skewed sample, or faithful representation of population):

  ;; selecting the neutral communicators:
  if (count SciMems with [skept-class = 1]) >= num-Neut-coms
  [
  ask n-of num-Neut-coms SciMems with [skept-class = 1]
  [set communicator-YN 1]
  ]
  ;; selecting the skeptic communicators:
  if (count SciMems with [skept-class = 0]) >= num-Skept-coms
  [
  ask n-of num-Skept-coms SciMems with [skept-class = 0]
  [set communicator-YN 1]
  ]

end

to communicate-consensus
  ;; Scientists transmit an LR based on their current mean data-point (see update-from-data for function for how LR is calculated)
  ; At the moment, scientists either communicate to the scientific community, or to the population
  let numcomcounter 0
  let numcomcounter2 0

  if Com-Type = 1
  [; Scientists communicating to each other
  while [numcomcounter < numSciMembers] ; will go through each possible receiver and allocate them a communicated LR
  [; grabbing the LR from a selected communicator (picked randomly)
    let passed-Slope [todays-Slope] of one-of SciMems with [communicator-YN = 1]
   ; passing to a random community member (who hasn't yet received anything this round)
    ask one-of SciMems with [received-YN = 0]
    [set received-Slope passed-Slope
      set received-YN 1]
  set numcomcounter (numcomcounter + 1)
  ]
  ]
  if Com-Type = 2
  [; Scientists communicating to population
  while [numcomcounter < numComMembers] ; will go through each possible receiver and allocate them a communicated LR
  [; grabbing the LR from a selected communicator (picked randomly)
    let passed-LR [todays-LR] of one-of SciMems with [communicator-YN = 1]
   ; passing to a random community member (who hasn't yet received anything this round)
    ask one-of ComMems with [received-YN = 0]
    [set received-LR passed-LR
      set received-YN 1]
  set numcomcounter (numcomcounter + 1)
  ]
  ]
  if Com-Type = 3
  [; Scientists communicating to both each other and population
  while [numcomcounter < numComMembers] ; will go through each possible receiver and allocate them a communicated LR
  [; grabbing the LR from a selected communicator (picked randomly)
    let passed-LR [todays-LR] of one-of SciMems with [communicator-YN = 1]
   ; passing to a random community member (who hasn't yet received anything this round)
    ask one-of ComMems with [received-YN = 0]
    [set received-LR passed-LR
      set received-YN 1]
  set numcomcounter (numcomcounter + 1)
  ]
  while [numcomcounter2 < numSciMembers] ; will go through each possible receiver and allocate them a communicated LR
  [; grabbing the LR from a selected communicator (picked randomly)
    let passed-Slope [todays-Slope] of one-of SciMems with [communicator-YN = 1]
   ; passing to a random community member (who hasn't yet received anything this round)
    ask one-of SciMems with [received-YN = 0]
    [set received-Slope passed-Slope
      set received-YN 1]
  set numcomcounter2 (numcomcounter2 + 1)
  ]
  ]
end

;; SETUP / INITIALISATION
to setup
  ;; set up and calls to various initialization procedures from here.
  clear-all

  set ColdData csv:from-file "CRUTTempData.csv" ;; remove number to go back to raw data (not windowed difference in aberations)
  set HotData csv:from-file "GISTempData.csv"

  ;; Neutral scientist creation
  create-SciMems ((numSciMembers / 100) * (100 - skepticProp))
    [ setxy ((random (2 * max-pxcor)) + min-pxcor)
            ((random (2 * max-pycor)) + min-pycor)
      set shape "circle"
      set size .5
      ;; PRIOR:
      set p-CC 0.01;; current belief in climate change

      ;; Setting key variables
      ;; Neutral Scientist
        set skept-class 1
        set mem-cap B-mem-Cap
        set LR-scale-val B-LR-scale-Shift
        set data-set-pref B-data-Pref
        set color blue

      ;; Memory "window"? Short-termism likely to lead to lower perception of long-term trends
      ; This can be manipulated via a "proportion" set in agent simulation parameters
      ;set mem-cap 100;; Agent memory cap (shorter / longer)

      ;; Does the agent have a "skewed" or "faithful" interpretation of evidence...
      ; Again, can be manipulated via a proportion set in agent simulation parameters (binary randomization)
      ;set LR-scale-val 1;; scaling shift for LR scale (skewed (0)/ faithful(1))

      ;; Does the agent sample (or "tend" to sample) from the cold or hot database..
      ; Again, can be manipulated via a proportion set in agent simulation parameters (binary randomization)
      ;set data-set-pref 1;; cold (0) / hot (1) dataset choice

      ;; Initial Agent Memory (do we want an initial sample here? probably).
      set data-mem [];; agents memory list (forms distribution)
      let datamem-counter 1
      while [datamem-counter < InitialSampleSize]
      [
      if data-set-pref = 1 ;; hot data set preference
      [let hotyearData item (StartYear - 1880 + datamem-counter) HotData ;; reading off year row
       let hotyearTemp item 1 hotyearData ;; reading off temp for that year
       set data-mem lput hotyearTemp data-mem;; adding the temperature point to agent memory
      ]
      if data-set-pref = 0 ;; cold data set preference
      [let coldyearData item (StartYear - 1880 + datamem-counter) ColdData ;;reading off year row
       let coldyearTemp item 1 coldyearData ;;reading off temp for that year
       set data-mem lput coldyearTemp data-mem;; adding the temperature point to agent memory
      ]
      set datamem-counter (datamem-counter + 1)
      ]
      ;; These can be set after sampling, or can be placeholders until first update round.
      set data-mem-mean mean data-mem;; mean for agent memory distribution
      set data-mem-sd standard-deviation data-mem;; sd for agent memory distribution

      ;; Communication parameter:
      set communicator-YN 0
      set received-YN 0
    ]

  ;; Skeptic scientist creation
  create-SciMems ((numSciMembers / 100) * skepticProp)
    [ setxy ((random (2 * max-pxcor)) + min-pxcor)
            ((random (2 * max-pycor)) + min-pycor)
      set shape "circle"
      set size .5
      ;; PRIOR:
      set p-CC 0.01;; current belief in climate change

      ;; Setting key variables
        ;; Skeptic Scientist
        set skept-class 0
        set mem-cap S-mem-Cap
        set LR-scale-val S-LR-scale-Shift
        set data-set-pref S-data-Pref
        set color blue

      ;; Memory "window"? Short-termism likely to lead to lower perception of long-term trends
      ; This can be manipulated via a "proportion" set in agent simulation parameters
      ;set mem-cap 100;; Agent memory cap (shorter / longer)

      ;; Does the agent have a "skewed" or "faithful" interpretation of evidence...
      ; Again, can be manipulated via a proportion set in agent simulation parameters (binary randomization)
      ;set LR-scale-val 1;; scaling shift for LR scale (skewed (0)/ faithful(1))

      ;; Does the agent sample (or "tend" to sample) from the cold or hot database..
      ; Again, can be manipulated via a proportion set in agent simulation parameters (binary randomization)
      ;set data-set-pref 1;; cold (0) / hot (1) dataset choice

      ;; Initial Agent Memory (do we want an initial sample here? probably).
      set data-mem [];; agents memory list (forms distribution)
      let datamem-counter 1
      while [datamem-counter < InitialSampleSize]
      [
      if data-set-pref = 1 ;; hot data set preference
      [let hotyearData item (StartYear - 1880 + datamem-counter) HotData ;; reading off year row
       let hotyearTemp item 1 hotyearData ;; reading off temp for that year
       set data-mem lput hotyearTemp data-mem;; adding the temperature point to agent memory
      ]
      if data-set-pref = 0 ;; cold data set preference
      [let coldyearData item (StartYear - 1880 + datamem-counter) ColdData ;;reading off year row
       let coldyearTemp item 1 coldyearData ;;reading off temp for that year
       set data-mem lput coldyearTemp data-mem;; adding the temperature point to agent memory
      ]
      set datamem-counter (datamem-counter + 1)
      ]
      ;; These can be set after sampling, or can be placeholders until first update round.
      set data-mem-mean mean data-mem;; mean for agent memory distribution
      set data-mem-sd standard-deviation data-mem;; sd for agent memory distribution

      ;; Communication parameter:
      set communicator-YN 0
      set received-YN 0
    ]

  create-ComMems numComMembers
    [ setxy ((random (2 * max-pxcor)) + min-pxcor)
            ((random (2 * max-pycor)) + min-pycor)
      set shape "circle"
      set size .5
      set color blue
      ;; PRIOR:
      set p-CC 0.01;; current belief in climate change
      set mem-cap 10
      set LR-scale-val 1
      set data-set-pref 2

      ;; Initial Agent Memory (do we want an initial sample here? probably).
      set data-mem [];; agents memory list (forms distribution)

      ;; These can be set after sampling, or can be placeholders until first update round.
      set data-mem-mean 0;; mean for agent memory distribution
      set data-mem-sd 1;; sd for agent memory distribution

      ;; Communication parameter:
      set received-YN 0
    ]

  global-initialization
  reset-ticks
end


;;===================================================
;; UTILITY PROCECDURES
;;===================================================

to global-initialization
  set yearCount ((StartYear - 1879) + InitialSampleSize)

  set ColdPlotter []
  set HotPlotter []
  let hotyearData item yearCount HotData ;;reading off year row
  let hotyearTemp item 1 hotyearData
  set HotPlotter lput hotyearTemp HotPlotter
  let coldyearData item yearCount ColdData ;;reading off year row
  let coldyearTemp item 1 coldyearData
  set ColdPlotter lput coldyearTemp ColdPlotter

  if skepticProp < 100
  [set g-ph-sci mean [p-CC] of SciMems with [skept-class = 1]; global reporter for neutral scientists (mean p-h)
  set v-ph-sci variance [p-CC] of SciMems with [skept-class = 1]]; global reporter for neutral scientists (var p-h)
  if skepticProp > 0
  [set g-ph-den mean [p-CC] of SciMems with [skept-class = 0]; global reporter for skeptic scientists (mean p-h)
  set v-ph-den variance [p-CC] of SciMems with [skept-class = 0]]; global reporter for skeptic scientists (var p-h)
  set g-ph-pop mean [p-CC] of ComMems; global reporter for skeptic scientists (mean p-h)
  if numComMembers > 1
  [set v-ph-pop variance [p-CC] of ComMems]; global reporter for skeptic scientists (var p-h)

  ;; DEFUNCT
  ;set global-vote-tally mean [own-vote-tally] of turtles
end
@#$#@#$#@
GRAPHICS-WINDOW
623
18
930
587
-1
-1
1.862
1
10
1
1
1
0
0
0
1
-80
80
-150
150
0
0
1
ticks
30.0

BUTTON
2
25
65
58
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
71
25
134
58
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
141
25
216
58
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
53
65
203
83
System Parameterization
11
0.0
1

TEXTBOX
5
10
208
28
Initialization and Simulation Commands
11
0.0
1

PLOT
252
18
621
252
Current Belief in Climate Change
Time
P(H)
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"SciMem Overall" 1.0 0 -16777216 true "" "plot mean [p-CC] of SciMems"
"ComMem Overall" 1.0 0 -7500403 true "" "plot mean [p-CC] of ComMems"
"SciMem Deniers" 1.0 0 -2674135 true "" "plot mean [p-CC] of SciMems with [skept-class = 0]"
"SciMem Neutral" 1.0 0 -13791810 true "" "plot mean [p-CC] of SciMems with [skept-class = 1]"

SLIDER
115
79
249
112
numSciMembers
numSciMembers
1
10000
1000.0
1
1
NIL
HORIZONTAL

SLIDER
157
114
249
147
skepticProp
skepticProp
0
100
3.0
1
1
NIL
HORIZONTAL

SLIDER
134
149
249
182
InitialSampleSize
InitialSampleSize
1
100
3.0
1
1
NIL
HORIZONTAL

SLIDER
119
254
250
287
numComMembers
numComMembers
1
10000
1000.0
1
1
NIL
HORIZONTAL

SLIDER
117
218
250
251
ticks-to-Data-Sample
ticks-to-Data-Sample
1
100
6.0
1
1
NIL
HORIZONTAL

TEXTBOX
17
333
106
351
Skeptic Definition
11
0.0
1

TEXTBOX
254
330
352
348
Neutral Definition
11
0.0
1

SLIDER
3
350
116
383
S-mem-Cap
S-mem-Cap
1
100
3.0
1
1
NIL
HORIZONTAL

SLIDER
239
347
354
380
B-mem-Cap
B-mem-Cap
1
100
15.0
1
1
NIL
HORIZONTAL

SLIDER
3
385
116
418
S-data-Pref
S-data-Pref
0
1
0.0
1
1
NIL
HORIZONTAL

SLIDER
239
383
354
416
B-data-Pref
B-data-Pref
0
1
1.0
1
1
NIL
HORIZONTAL

SLIDER
3
419
116
452
S-LR-scale-Shift
S-LR-scale-Shift
-2
2
0.01
.001
1
NIL
HORIZONTAL

SLIDER
239
417
353
450
B-LR-scale-Shift
B-LR-scale-Shift
-2
2
0.0
.001
1
NIL
HORIZONTAL

MONITOR
490
293
600
338
Neutral Scientists
mean [p-CC] of SciMems with [skept-class = 1]
5
1
11

MONITOR
490
340
600
385
Skeptic Scientists
mean [p-CC] of SciMems with [skept-class = 0]
5
1
11

TEXTBOX
106
496
256
514
Communication Parameters
11
0.0
1

SLIDER
155
516
247
549
comFreq
comFreq
1
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
155
552
269
585
num-Skept-coms
num-Skept-coms
1
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
156
588
271
621
num-Neut-coms
num-Neut-coms
1
100
90.0
1
1
NIL
HORIZONTAL

MONITOR
490
387
600
432
Community
mean [p-CC] of ComMems
5
1
11

TEXTBOX
9
90
110
108
Number of Scientists
11
0.0
1

TEXTBOX
2
122
156
140
Percentage of Skeptic Scientists
11
0.0
1

TEXTBOX
20
152
120
180
Initial data-points in scientist memory
11
0.0
1

TEXTBOX
4
217
115
245
Frequency of scientist data sampling
11
0.0
1

TEXTBOX
14
256
119
285
Number of General Public
11
0.0
1

TEXTBOX
126
358
236
376
Data-points in memory
11
0.0
1

TEXTBOX
126
395
276
413
Dataset pref (1 = hot)
11
0.0
1

TEXTBOX
502
263
592
291
P(ClimateChange) Reporters:
11
0.0
1

TEXTBOX
120
421
239
449
LR Bias (0 = neutral)\nMore pos = more skeptic
11
0.0
1

TEXTBOX
127
305
229
323
Scientist Parameters
11
0.0
1

TEXTBOX
13
517
141
545
Communication Frequency (1 = every non-data tick)
11
0.0
1

TEXTBOX
9
555
148
583
Absolute number of skeptics in communication pool
11
0.0
1

TEXTBOX
8
593
151
621
Absolute number of neutrals in communication pool
11
0.0
1

TEXTBOX
2
625
152
667
Scientists communicate to each other (1) or to the population (2) or to both (3)
11
0.0
1

TEXTBOX
624
589
889
641
Visualisation Note:\nColors of agents change from blue towards red as P(H) increases.\n
11
0.0
1

SLIDER
156
623
328
656
Com-Type
Com-Type
1
3
1.0
1
1
NIL
HORIZONTAL

TEXTBOX
2
192
122
210
Year of Active Updating
11
0.0
1

SLIDER
120
183
249
216
StartYear
StartYear
1880
2017
1950.0
1
1
NIL
HORIZONTAL

PLOT
277
453
612
621
Scientist view of data
Time
Temp
0.0
10.0
-1.0
1.0
true
true
"" ""
PENS
"Skeptic Data" 1.0 0 -2674135 true "" "plot last ColdPlotter"
"Neutral Data" 1.0 0 -13345367 true "" "plot last HotPlotter"

@#$#@#$#@
## WHAT IS IT?

Climate Change Consensus Model

*Written by Toby D. Pilditch, for a project in collaboration with Stephan Lewandowsky and Jens Koed Madsen.*

## HOW IT WORKS

*Rough for now, will flesh out later.*

### SETUP:
- Datasets read in from csv files
- Scientists created, based on number specified in system parameters
- Scientists are either skeptics or neutral (defined in terms of memory window [shorter / longer], dataset preference [colder / hotter], and Likelihood Ratio scaling [neutral / biased].
- All scientists read in a manipulated number of data points to form their initial data distribution
- P(H) for climate change set for all as extremely low (.1%)
- General public created, based on number specified in system parameters
- Only require P(H) (they won't be looking at and storing data, only updating P(H) based on the Likelihoods communicated to them by scientists).

### RUN:
- Scientists sample from data (every X number of ticks)
- Scientists update memory distribution of data points
- Scientists use mean of updated distribution to calculate LR (influenced by LR-scaling parameter).
- Scientists update P(H) via Bayes theorem using this LR value
- Every Y ticks, scientists communicate this LR value directly, either too each other or to the general public. This is done by:
   -- Selection of skeptics and neutral scientists (based on system parameters) into the communicator "pool".
   -- Members of this pool randomly selected to communicate to random members (either of scientist or general public communities), until all have recieved 1 LR value.
   -- Communication recipients update their P(H) based on this recieved LR value.

- Simulation continues until data has finished being sampled (i.e. number of data points multiplied by frequency of sampling).

## HOW TO USE IT

*Setting varying proportions of skeptics, and shifting general LR scales "against" climate change.*

## THINGS TO NOTICE

*Cyclical nature of beliefs in climate change when skeptics are over-represented...*

## THINGS TO TRY

*Over-representation of skeptics in communicating to public.*

## EXTENDING THE MODEL

Many ways to extend the model, including the incorporation of media representation of scientists, 

## NETLOGO FEATURES

- Reading in CSVs
- Bayesian updating

## RELATED MODELS & REFERENCES

- Micro-targeted Campaigning MTC models (Madsen & Pilditch, 2017; Madsen & Pilditch, 2018)
- Echo Chamber Formation models (Madsen, Bailey, & Pilditch, 2017)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>(b-agents / numLearners) * 100</metric>
    <metric>(n-b-agents / numLearners) * 100</metric>
    <metric>((sqrt ((tick-tot-odd - tick-tot-even) ^ 2)) / numLearners) * 100</metric>
    <enumeratedValueSet variable="SC-bel-prop">
      <value value="1"/>
      <value value="1.1"/>
      <value value="1.2"/>
      <value value="1.3"/>
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numLearners">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="res-cave">
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CB-mean">
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SC-com-prop">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numInitBel">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxLinks">
      <value value="12"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
