(in-package RC)

;
;Update version 1.1
;
;Update version 1.3 (Stockholm 18/8 2002)
; Updated function: build-rhythm-pattern-wpauses (new), RC::r-pattern

(defun length-offset-in-voice-rule (layer1 layer2 offset)

  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
              (stop-other-layer 0))

         (cond ((= this-layer-nr layer1)  ;compare lengths, case 1
                (setf stop-other-layer (get-stop-time this-voice-nr layer2 (1- indexx)))
                (if (<= start-this-cell (+ stop-other-layer offset))
                  t
                  nil))
               ((= this-layer-nr layer2)  ;compare lengths, case 2
                (setf stop-other-layer (get-stop-time this-voice-nr layer1 (1- indexx)))
                (if (<= start-this-cell (- stop-other-layer offset))
                  t
                  nil))
               (t     ;bypass this rule
                t)
               ))
       )))



(defun layer-in-order-in-voice-rule (layer1 layer2 endtime)

  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
              (stop-other-layer 0))

         (cond ((= this-layer-nr layer1)  ;compare lengths, case 1
                (setf stop-other-layer (get-stop-time this-voice-nr layer2 (1- indexx)))
                (if (< start-this-cell endtime)
                  t
                  (<= start-this-cell stop-other-layer)))
               ((= this-layer-nr layer2)  ;compare lengths, case 2
                (setf stop-other-layer (get-stop-time this-voice-nr layer1 (1- indexx)))
                (if (< stop-other-layer endtime)
                  nil
                  (<= start-this-cell stop-other-layer)))
               (t     ;bypass this rule
                t)
               ))
       )))


(defun layer-in-order-between-voices-rule (layer1 layer2 voice1 voice2 endtime)

  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
              (stop-other-layer 0))

         (cond ((and (= this-layer-nr layer1)
                     (= this-voice-nr voice1))  ;compare lengths, case 1
                (setf stop-other-layer (get-stop-time voice2 layer2 (1- indexx)))
                (if (< start-this-cell endtime)
                  t
                  (<= start-this-cell stop-other-layer)))
               ((and (= this-layer-nr layer2)
                     (= this-voice-nr voice2))  ;compare lengths, case 2
                (setf stop-other-layer (get-stop-time voice1 layer1 (1- indexx)))
                (if (< stop-other-layer endtime)
                  nil
                  (<= start-this-cell stop-other-layer)))
               (t     ;bypass this rule
                t)
               ))
       )))


(om::defmethod! RC::r-layerorder  ((layer1 integer)
                                   (layer2 integer)
                                   (endtime number))
   :initvals '(1 2 0)
   :indoc '("layernr" "layernr" "duration")
   :doc "Rule for forcing one layer in a voice to be calculated before another
layer in the same voice up to a certain time point. After this point
the rule works in the same way as r-eqlength.

<layer1> is the number for the layer that should be calculated first.
<layer2> is the number for the layer that should be calculated after the
first layer.
<endtime> is the point to which the first layer should reach before starting
calculating the second layer. This is defined as a note value (ratio).

The rule also works with layer 0 (the time signatures).
--------------------------
Regel fr att tvinga ett skikt i en stmma att kalkyleras fre ett
annat skikt i samma stmma upp till en bestmd punkt. Drefter
fungerar regeln p samma stt som r-eqlength.

<layer1> r numret fr skiktet som berknas frst.
<layer2> r numret fr skiktet som sedan berknas.
<endtime> r punkten dit det frsta skiktet ska ha ntt innan
det andra skiktet brjar berknas. Anges som notvrde (ratio).

Regeln fungerar ocks fr skikt 0 (taktarterna).

"

   :icon 352

   (layer-in-order-in-voice-rule layer1 layer2 endtime)
   )

(om::defmethod! RC::gr-layerorder  ((layer1 integer)
                                    (voice1 integer)
                                    (layer2 integer)
                                    (voice2 integer)
                                    (endtime number))
   :initvals '(1 0 2 1 0)
   :indoc '("layernr" "voicenr" "layernr" "voicenr" "duration")
   :doc "Rule for forcing one layer in a voice to be calculated before another
layer in another voice up to a certain time point. After this point
the rule works in the same way as r-eqlength.

<layer1> is the number for the layer that should be calculated first.
<voice1> is the voice number for the layer that should be calculated first.
<layer2> is the number for the layer that should be calculated after the
first layer.
<voice2> is the voice number for the layer that should be calculated after the
first layer.
<endtime> is the point to which the first layer should reach before starting
calculating the second layer. This is defined as a note value (ratio).

The rule also works with layer 0 (the time signatures).

The rule has to be connected to the input for both layers on Òrules->pmcÓ.
--------------------------
Regel fr att tvinga ett skikt i en stmma att kalkyleras fre ett
annat skikt i en annan stmma upp till en bestmd punkt. Drefter
fungerar regeln p samma stt som r-eqlength.

<layer1> r numret fr skiktet som berknas frst.
<voice1> r numret fr stmman fr skiktet som berknas frst.
<layer2> r numret fr skiktet som sedan berknas.
<voice2> r numret fr stmman fr skiktet som sedan berknas.
<endtime> r punkten dit det frsta skiktet ska ha ntt innan
det andra skiktet brjar berknas. Anges som notvrde (ratio).

Regeln fungerar ocks fr skikt 0 (taktarterna).

Regeln mste kopplas till ingngen fr de bda skikten p Òrules->pmcÓ.

"

   :icon 352

   (layer-in-order-between-voices-rule layer1 layer2 voice1 voice2 endtime)
   )



;---------------

(defun barline-syncope-rule (rhythm-layer-nr allowed-syncopations)

  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x)))

         (cond ((= this-layer-nr 0)
                (let* ((start-this-variable (get-stop-time this-voice-nr 0 (1- indexx)))
                       (rhythm-layer-onset-times (get-one-rhythmlayer this-voice-nr rhythm-layer-nr (1- indexx)))
                       (first-onset-time (car (member start-this-variable rhythm-layer-onset-times :test '<=))))
                  (if first-onset-time
                    (member (- first-onset-time start-this-variable) allowed-syncopations)
                    t)))
               ((= this-layer-nr rhythm-layer-nr)
                (let* ((start-this-variable (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                       (stop-this-variable (+ start-this-variable (get-variabledur x)))
                       (this-variable-global-onset (mapcar #'(lambda (onset) (+ onset start-this-variable)) (get-local-onset x)))
                       (measure-layer (if (member 0 (detect-fix-layer this-voice-nr))
                                        (decode-measure-fixlayers this-voice-nr) ; presetlayers will output 20 extra measures at the end.
                                        (if (check-if-measure-domain? this-voice-nr)
                                          (get-one-measurelayer this-voice-nr 0 indexx)
                                          nil)))
                       (measures-starttime (om::dx->x 0 (mapcar #'(lambda (x) (apply '/ x)) measure-layer)))
                       (first-measure-in-section (first (fast-lp-filter start-this-variable measures-starttime)))
                       (measures-starttime-in-section
                        (reverse (fast-band-filter first-measure-in-section stop-this-variable measures-starttime)))
                       (rhythm-layer-onset-times (append (get-one-rhythmlayer this-voice-nr rhythm-layer-nr (1- indexx))
                                                         (cdr this-variable-global-onset))) ; the current variable is yet not stored in the vector
                       )
                  (if rhythm-layer-onset-times
                    (let ((syncopations-over-barlines
                           (mapcar #'(lambda (measure-start)
                                       (- (car (member measure-start rhythm-layer-onset-times :test '<=)) measure-start))
                                   measures-starttime-in-section)))
                      (subsetp syncopations-over-barlines allowed-syncopations))

                    t)

                  ))
               (t t)) ;the layer the variable belongs to is not included in this rule
         ))
   ))





(defun barline-syncope-rule-ok-empty (rhythm-layer-nr allowed-syncopations)

  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x)))

         (cond ((= this-layer-nr 0)
                (let* ((start-this-variable (get-stop-time this-voice-nr 0 (1- indexx)))
                       (rhythm-layer-onset-times (get-one-rhythmlayer this-voice-nr rhythm-layer-nr (1- indexx)))
                       (first-onset-time (car (member start-this-variable rhythm-layer-onset-times :test '<=))))
                  (if (and first-onset-time (< (- first-onset-time start-this-variable) (get-variabledur x)))
                    (member (- first-onset-time start-this-variable) allowed-syncopations)
                    t)))
               ((= this-layer-nr rhythm-layer-nr)
                (let* ((start-this-variable (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                       (stop-this-variable (+ start-this-variable (get-variabledur x)))
                       (this-variable-global-onset (mapcar #'(lambda (onset) (+ onset start-this-variable)) (get-local-onset x)))
                       (measure-layer (if (member 0 (detect-fix-layer this-voice-nr))
                                        (decode-measure-fixlayers this-voice-nr) ; presetlayers will output 20 extra measures at the end.
                                        (if (check-if-measure-domain? this-voice-nr)
                                          (get-one-measurelayer this-voice-nr 0 indexx)
                                          nil)))
                       (measures-starttime (butlast (om::dx->x 0 (mapcar #'(lambda (x) (apply '/ x)) measure-layer))))
                       (first-measure-in-section (first (fast-lp-filter start-this-variable measures-starttime)))
                       (measures-starttime-in-section
                        (reverse (fast-band-filter first-measure-in-section stop-this-variable measures-starttime)))
                       (rhythm-layer-onset-times (append (get-one-rhythmlayer this-voice-nr rhythm-layer-nr (1- indexx))
                                                         (cdr this-variable-global-onset))) ; the current variable is yet not stored in the vector
                       )

                  (if rhythm-layer-onset-times
                    (let* ((syncopations-over-barlines
                            (mapcar #'(lambda (measure-start)
                                        (- (car (member measure-start rhythm-layer-onset-times :test '<=)) measure-start))
                                    measures-starttime-in-section))  ; check where first event in all measures occure (offset from measure start)
                           (filtered-syncopations-over-barlines     ; filter out all cases when no onsettime occure in a measure
                            (remove nil
                                    (mapcar #'(lambda (syncope measure-length) (if (and measure-length (< syncope measure-length)) syncope nil))
                                            syncopations-over-barlines
                                            (mapcar #'(lambda (m-starttime) (if measure-layer
                                                                              (apply '/ (nth (position m-starttime measures-starttime) measure-layer))
                                                                              nil))
                                                    measures-starttime-in-section)))))
                      (subsetp filtered-syncopations-over-barlines allowed-syncopations))

                    t)

                  ))
               (t t)) ;the layer the variable belongs to is not included in this rule
         ))
   ))




(om::defmethod! RC::r-sync-over-barline ((rhythm-layer-nr integer)
                                         (allowed-syncopes list)
                                         &optional (special-case 'no-empty-m))
   :initvals '(1 '(0) 'no-empty-m)
   :indoc '("rhythmlayer" "notevalues" "empty-measures?")
   :menuins '((2 (("no-empty-measures" 'no-empty-m) ("empty-measures" 'empty-m))))
   :doc "Rule for controlling the position of the first event  after a bar line, i.e. control
of syncopations over bar lines.

<rhythm-layer-nr> is the number for the rhythm layer the rule is valid for.
<allowed-syncopes> is a list of possible positions for the first
event in a measure. They are defined as offsets (note value as a ratio)
from the bar line.
<special-case> is a popup menu where you can choose if it is allowed
that a measure has no events, i.e. that no starting points for events
occur in the measure.

Ex: If the second input is '(0 1/8), and the option no-empty-measures is
chosen in the third input, every new measure must either start with a new
event at the first beat, or have the first event starting one eight note
after the bar line (the first eight note in the measure is then slured from the
previous measure).
--------------------------
Regel fr att kontrollera positionen fr den frsta hndelsen (not eller paus)
efter ett taktstreck, d.v.s. kontroll av synkoper ver taktstreck.

<rhythm-layer-nr> r numret p det rytmskikt som regeln gller fr.
<allowed-syncopes> r en lista med mjliga positioner fr den
frsta hndelsen efter ett taktstreck. De r definierade som
en offset (notvrde som ett brk) frn taktstrecket.
<special-case> r en popup meny dr man kan vlja om det r
tilltet att en takt inte har ngra hndelser, d.v.s. att
inga startpunkter fr hndelser intrffar i takten.

Ex: Om den andra ingngen r '(0 1/8), och no-empty-measures r vald
p tredje ingngen, mste varje ny takt antingen starta med en ny
hndelse p frsta slaget, eller ha frsta hndelsen en ttondelsnot
efter taktstrecket (den frsta ttondelsnoten r d verbunden frn
fregende takt).

"

   :icon 352

   (case special-case
     ((no-empty-m)(barline-syncope-rule rhythm-layer-nr allowed-syncopes))
     ((empty-m)(barline-syncope-rule-ok-empty rhythm-layer-nr allowed-syncopes)))
   )


(om::defmethod! RC::get-voicenumber ((x t))
   :initvals '(nil)
   :indoc '("search-var")
   :doc "Get the voice number for the current instantiated variable.

<x> is the current variable. It should be connected to the second
input inside a user rule patch.
--------------------------
Hmta \"voice\" numret fr den aktuella instancierade variabeln.

<x> r den aktuella variabeln. Den ska ansutas till den andra ingngen
inuti en patch fr en anvndardefinierad regel.
"
   :icon 367

   (get-voice-nr x)
   )




;;------------
;; Function suggested by Mauro Lanza

(defun build-rhythm-pattern (number-of-events-in-pattern layer-nr)
  (list
   #'(lambda (indexx x)
       (let* ((voice-nr (get-voice-nr x))
              (this-layer-nr (get-layer-nr x))
              (rhythm-cell (get-rhythmcell x))
              (all-earlier-cells (get-all-rhythmcells-in-layer voice-nr layer-nr (1- indexx)))
              (rhythm-cue-cell (nth (1- number-of-events-in-pattern) (reverse all-earlier-cells))))


         (if (and (= this-layer-nr layer-nr) (>= (length all-earlier-cells) number-of-events-in-pattern))
           (equal (mapcar 'abs rhythm-cell) rhythm-cue-cell) ; abs will remove minus signs for pauses. Thus, the rule does
           ;                                                   not understand the difference between a note and a pause (not possible in another way).
           t))))
  )


(defun build-rhythm-pattern-wpauses (number-of-events-in-pattern layer-nr)
  (list
   #'(lambda (indexx x)
       (let* ((voice-nr (get-voice-nr x))
              (this-layer-nr (get-layer-nr x))
              (rhythm-cell (get-rhythmcell x))

              (all-earlier-cells (get-all-rhythmcells-in-layer voice-nr layer-nr (1- indexx)))
              (all-earlier-pause-flags (get-all-rhythmcell-pauseflags-in-layer voice-nr layer-nr (1- indexx)))
              (rhythm-cue-cell (nth (1- number-of-events-in-pattern) (reverse (om::om* all-earlier-cells
                                                                                       all-earlier-pause-flags)))))


         (if (and (= this-layer-nr layer-nr) (>= (length all-earlier-cells) number-of-events-in-pattern))
           (equal rhythm-cell rhythm-cue-cell)
           t))))
  )


(defun check-duration-rhythm-pattern (number-of-events-in-pattern duration layer-nr)
  (list
   #'(lambda (indexx x)
       (let* ((voice-nr (get-voice-nr x))
              (this-layer-nr (get-layer-nr x))
              (rhythm-cell (get-rhythmcell x))
              (all-earlier-cells (get-all-rhythmcells-in-layer voice-nr layer-nr (1- indexx)))
              (one-pattern (append rhythm-cell
                                   (apply 'append
                                          (last all-earlier-cells (1- number-of-events-in-pattern))))))


         (if (and (= this-layer-nr layer-nr)
                  (or (>= (get-stop-time voice-nr this-layer-nr (1- indexx)) duration)
                      (>= (length all-earlier-cells) number-of-events-in-pattern))
                  ; this will give true also if there are too
                  ; few variables which gives the sum "duration" in the beginning, but this will be filtered out in the next variable.
                  )
           (= duration (apply '+ (mapcar 'abs one-pattern)))
           t))))
  )



(om::defmethod! RC::r-pattern  ((layer integer)
                                (nr-of-ev integer)
                                &optional (duration nil))
   :initvals '(1 4 nil)
   :indoc '("layernr" "#evts" "durvalue")
   :doc "Rule for building a pattern.

<layer> is the number for the rhythm layer the pattern will be built.
<nr-of-ev> is the number of events in the pattern.
<duration> is the length of the pattern.

Ex: If nr-of-ev is 4 and duration is 1, then a pattern with
the total length of a whole note with four rhythm events in
it will be built (and repeated over and over again) in the
layer.


--------------------------
Regel fr att bygga ett pattern.

<layer> r numret p det rytmskikt som \"mnstret\" ska byggas.
<nr-of-ev> r antalet rytmiska hndelser (noter eller pauser) i mnstret.
<duration> r mnstrets lngd.

Ex: Om nr-of-ev r 4 och duration 1, s kommer ett mnster med
den totala lngden en helnot med 4 rytmhndelser att byggas
(och upprepas i ondlighet) i skiktet.

"

   :icon 352
   (case duration
     ((nil) (build-rhythm-pattern-wpauses nr-of-ev layer))
     (t (append (build-rhythm-pattern-wpauses nr-of-ev layer)
                (check-duration-rhythm-pattern nr-of-ev duration layer))))

   )
