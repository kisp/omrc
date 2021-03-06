;****************************
;Rhythm Constraints library version 1.0 by rjan Sandred, IRCAM 1999
;

(in-package RC)
;Measure layer is always 0

(defun measure-rule (rhythm-layer-nr allowed-subdivisions beatvalue)
  (let ((1beat-grid (create-local-grid allowed-subdivisions (list 1 beatvalue))))
    (list
     (list
      (list
       (list beatvalue 1beat-grid))
      #'(lambda (indexx x)
          (let* ((this-layer-nr (get-layer-nr x))
                 (this-voice-nr (get-voice-nr x)))

            (cond ((= this-layer-nr 0)                             ;check new grid towards rhythms
                   (if (= beatvalue (cadr (get-timesign x)))       ;but only if this is the beat value the rule is for
                     (let*
                       ((start-this-variable (get-stop-time this-voice-nr 0 (1- indexx)))
                        (stop-this-variable (+ start-this-variable (get-variabledur x)))
                        (nr-of-beats (car (get-timesign x)))
                        (beatlist-grid (apply 'append (make-list nr-of-beats :initial-element 1beat-grid)))
                        (rhythm-layer-onset-times (get-one-rhythmlayer this-voice-nr rhythm-layer-nr (1- indexx)))
                        (this-local-grid-abstime (om::dx->x start-this-variable beatlist-grid)))
                       (subsetp
                        (fast-band-filter start-this-variable stop-this-variable rhythm-layer-onset-times)
                        this-local-grid-abstime))
                     t))
                  ((= this-layer-nr rhythm-layer-nr)                     ;check new rhythm towards grid
                   (let* ((start-this-variable (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                          (global-grid (get-timegrid-layer this-voice-nr 0 (1- indexx)))
                          (this-local-onset-abstime (mapcar #'(lambda (x) (+ start-this-variable x)) (get-local-onset x))))
                     (subsetp
                      (fast-lp-filter (get-stop-time this-voice-nr 0 (1- indexx)) this-local-onset-abstime)
                      global-grid)))
                  (t t)) ;the layer the variable belongs to is not included in this rule
            ))))
    ))


;*********************
;om function

(om::defmethod! RC::r-beat-subdiv  ((rhythm-layer-nr integer)
                                     (beatvalue1 integer)
                                     (subdiv1 list)
                                     &optional beatvalue2 subdiv2 beatvalue3 subdiv3
                                     beatvalue4 subdiv4 beatvalue5 subdiv5)
  :initvals '(1 4 '(1 2 3 4 5) 8 '(1 2 3) 2 '(1 2 3) 16 '(1 2 3) 32 '(1 2 3))
  :indoc '("rhythmlayer" "beatvalue" "subdiv-list" "beatvalue" "subdiv-list"
           "beatvalue" "subdiv-list" "beatvalue" "subdiv-list" "beatvalue" "subdiv-list")
  :doc "Rule for controlling the subdivisions of beats in measures.

<rhythm-layer-nr> is the number for the rhythm layer the rule is valid for.
<beatvalue1> and <subdiv1> come in a pair (can be expanded up to maximum 5
pairs). <subdiv1> is a list of possible subdivisions of the beat in a measure.
<beatvalue1> is the length of the beat (i.e. the lower number in the time
signature) that is referred to.

Example: If <beatvalue1> = 4, then <subdiv1> tells how the beats in a measure
with a time signature with the beat length a quarter note (for example 4//4,
3//4) can be subdivided. If <subdiv1> = (1 2 3 4 5) this means that the beat
can be subdivided down to maximum a quintuplet, however only if this is on
the beat (i.e. not as a syncopation). In this example the rule will not affect
measures with time signatures that have for example eight-note as the beat length;
every beat length has to be defined separately (in pairs of <betvalue> and
<subdiv>).
--------------------------
Regel fr att kontrollera underdelningarna av pulsslag i takter.

<rhythm-layer-nr> r numret p det rytmskikt som regeln gller fr.
<beatvalue1> och <subdiv1> hr ihop i par (kan expanderas upp till maximalt 5
par). <subdiv1> r en lista av mjliga underdelningar av ett pulsslag i en
takt. <beatvalue1> r lngden p pulsslaget (d.v.s. den undre siffran i
taktartssignaturen) som avses.

Exempel: Om <beatvalue1> = 4 anger <subdiv1> hur pulsslagen i en taktart med
pulsvrdet en fjrdedelsnot (t.ex. 4//4, 3//4) kan underdelas. Om
<subdiv1> = (1 2 3 4 5) betyder det att pulsslaget kan underdelas ner till
maximalt en kvintol, dock endast om denna hamnar p slaget (d.v.s. inte som
synkop). I exemplet pverkar inte denna regel takter med taktarter som har
t.ex. ttondelsnot som pulsvrde; varje pulsvrde mste definieras separat
(i par om <beatvalue> och <subdiv>).
"
  :icon 354

  (append
   (if beatvalue1
     (measure-rule rhythm-layer-nr subdiv1 beatvalue1))
   (if beatvalue2
     (measure-rule rhythm-layer-nr subdiv2 beatvalue2))
   (if beatvalue3
     (measure-rule rhythm-layer-nr subdiv3 beatvalue3))
   (if beatvalue4
     (measure-rule rhythm-layer-nr subdiv4 beatvalue4))
   (if beatvalue5
     (measure-rule rhythm-layer-nr subdiv5 beatvalue5)))
  )
