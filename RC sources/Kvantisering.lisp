;;;functions
(in-package RC)



(defun check-max-in-seq-old (rytmseq timeline max-deviation
                                 max-ornaments max-skips)
  (let ((nr-ornaments 0)
        (nr-skips 0))
    (loop while (and rytmseq (>= max-ornaments nr-ornaments) (>= max-skips nr-skips))
          do (if (<= (- (car rytmseq) max-deviation) (car timeline))
               (if (>= (+ (car rytmseq) max-deviation) (car timeline))
                 (progn (pop rytmseq) (pop timeline) (setf nr-ornaments 0) (setf nr-skips 0))
                 (progn (pop rytmseq) (incf nr-ornaments)))
               (progn (incf nr-skips) (pop timeline))))
    (and (>= max-ornaments nr-ornaments) (>= max-skips nr-skips))))

(in-package RC)

(defun check-max-in-seq (rytmseq timeline max-deviation rhythm-end
                                 max-ornaments max-skips)
  (let ((nr-ornaments 0)
        (nr-skips 0))
    (loop while (and rytmseq timeline (>= max-ornaments nr-ornaments) (>= max-skips nr-skips))
          do (if (<= (- (car rytmseq) max-deviation) (car timeline))
               (if (>= (+ (car rytmseq) max-deviation) (car timeline))
                 (progn (pop rytmseq) (pop timeline) (setf nr-ornaments 0) (setf nr-skips 0))
                 (progn (pop rytmseq) (incf nr-ornaments)))
               (progn (incf nr-skips) (pop timeline))))

    (loop while (and timeline (>= max-skips nr-skips) (> (- rhythm-end max-deviation) (car timeline)))
          do (progn (incf nr-skips) (pop timeline)))

    (and (>= max-ornaments nr-ornaments) (>= max-skips nr-skips))))




(defun quant-max-in-seq-rule (layer-nr timeline tempo max-deviation
                                       max-ornaments max-skips)
  (let ((max-deviation-sec (/ (* 240 max-deviation) tempo)))

    (list
     #'(lambda (indexx x)
         (if (= layer-nr (get-layer-nr x))
           (let* ((this-voice-nr (get-voice-nr x))
                  (start-this-cell (1+ (get-stop-time this-voice-nr layer-nr (1- indexx))))
                  (stop-this-cell (1- (+ start-this-cell (get-variabledur x))))
                  (this-glogal-cell (om::om* (om::om+ start-this-cell (get-local-onset x))
                                             (get-pauses x)))
                  (onsets-in-rhythm (remove-pauses2
                                     (append
                                      (om::om* (om::om+ 1 (get-one-rhythmlayer this-voice-nr layer-nr (1- indexx)))
                                               (get-one-layer-pauseflags this-voice-nr layer-nr (1- indexx)))
                                      this-glogal-cell)))
                  ;as in hierarchy-rule - see Rules.lisp
                  (onsets-sec-in-rhythm (om::om/ (om::om* 240 (om::om- onsets-in-rhythm 1)) tempo))
                  (stop-time (/ (* 240 stop-this-cell) tempo)))

             (check-max-in-seq onsets-sec-in-rhythm timeline max-deviation-sec stop-time
                               max-ornaments max-skips))
           t)))
    ))



(defun check-max-in-seq-and-total (rytmseq timeline max-deviation rhythm-end
                                           max-ornaments max-skips max-tot-ornaments max-tot-skips)
  (let ((nr-ornaments 0)
        (tot-nr-ornaments 0)
        (nr-skips 0)
        (tot-nr-skips 0))
    (loop while (and rytmseq timeline (>= max-ornaments nr-ornaments) (>= max-skips nr-skips)
                     (>= max-tot-ornaments tot-nr-ornaments) (>= max-tot-skips tot-nr-skips))
          do (if (<= (- (car rytmseq) max-deviation) (car timeline))
               (if (>= (+ (car rytmseq) max-deviation) (car timeline))
                 (let ((x)) (pop rytmseq) (pop timeline) (setf nr-ornaments 0) (setf nr-skips 0))
                 (let ((x)) (pop rytmseq) (incf nr-ornaments) (incf tot-nr-ornaments)))
               (let ((x)) (incf nr-skips) (incf tot-nr-skips) (pop timeline))))

    (loop while (and timeline (>= max-skips nr-skips) (>= max-tot-skips tot-nr-skips)
                     (> (- rhythm-end max-deviation) (car timeline)))
          do (progn (incf nr-skips) (pop timeline)))

    (and (>= max-ornaments nr-ornaments) (>= max-skips nr-skips)
         (>= max-tot-ornaments tot-nr-ornaments) (>= max-tot-skips tot-nr-skips))))




(defun quant-max-in-seq-and-total-rule (layer-nr timeline tempo max-deviation
                                                 max-ornaments max-skips
                                                 max-tot-ornaments max-tot-skips)

  (let ((max-deviation-sec (/ (* 240 max-deviation) tempo)))
    (list
     #'(lambda (indexx x)
         (if (= layer-nr (get-layer-nr x))
           (let* ((this-voice-nr (get-voice-nr x))
                  (start-this-cell (1+ (get-stop-time this-voice-nr layer-nr (1- indexx))))
                  (stop-this-cell (1- (+ start-this-cell (get-variabledur x))))
                  (this-glogal-cell (om::om* (om::om+ start-this-cell (get-local-onset x))
                                             (append (get-pauses x) '(1))))
                  (onsets-in-rhythm (remove-pauses2 (append
                                                     (om::om* (om::om+ 1 (get-one-rhythmlayer this-voice-nr layer-nr (1- indexx)))
                                                              (get-one-layer-pauseflags this-voice-nr layer-nr (1- indexx)))
                                                     this-glogal-cell)
                                                    ))
                  ;as in hierarchy-rule - see Rules.lisp
                  (onsets-sec-in-rhythm (om::om/ (om::om* 240 (om::om- onsets-in-rhythm 1)) tempo))
                  (stop-time (/ (* 240 stop-this-cell) tempo)))

             (check-max-in-seq-and-total onsets-sec-in-rhythm timeline max-deviation-sec stop-time
                                         max-ornaments max-skips max-tot-ornaments max-tot-skips))
           t)))
    ))

(om::defmethod! RC::r-quant ((layer-nr integer)
                             (timeline list)
                             (tempo number)
                             (max-deviation number)
                             (max-ornaments integer)
                             (max-skips integer)
                             (max-tot-ornaments integer)
                             (max-tot-skips integer))


   :initvals '(1 nil  60 1/32 0 0 0 0)
   :indoc '("layernr" "seconds" "bpm (quarter note)" "ratio" "number" "number" "number" "number")
   :doc "Rule for quantifying a sequence of events (onsets) to
proportional durations. The settings for ornaments and
skips will allow the quantification to be more flexible.

Pauses are not 100% supported.

<layer-nr> is the layer number for the layer that contains the quantified
           rhythm.
<timeline> is the sequence of events as a list of timevalues (seconds).
<tempo> is the tempo for the quantified rhythm (quarternotes per minute).
<max-deviation> is the maximum allowed deviation from the timeline
           (a notevalue as a ratio, for example 1/32).
<max-ornaments> is the maximum number of ornamental events in the
           quantified rhythm between two events in the timeline.
           If set to 0 every event in the quantified rhythm must
           correspond to an event in the timeline.
<max-skips> is the maximum number of events in immediate sequence
           in the timeline that can be ignored in the quantified rhythm.
           If set to 0 every event in the timeline must
           correspond to an event in the quantified rhythm.
<max-tot-ornaments> is the maximum number of all ornamental events in the
           whole answer.
<max-tot-skips> is the maximum number of events in the timeline that can
           be ignored in the quantified rhythm.
"
   :icon 352

   (quant-max-in-seq-and-total-rule layer-nr timeline tempo max-deviation
                                    max-ornaments max-skips
                                    max-tot-ornaments max-tot-skips))


;; inga ornament eller skips => 1, sedan minskar vrdet.

(defun heur-check-max-in-seq (rytmseq timeline max-deviation rhythm-end)

  (let ((nr-ornaments 0)
        (nr-skips 0))
    (loop while (and rytmseq timeline)
          do (if (<= (- (car rytmseq) max-deviation) (car timeline))
               (if (>= (+ (car rytmseq) max-deviation) (car timeline))
                 (progn (pop rytmseq) (pop timeline))
                 (progn (pop rytmseq) (incf nr-ornaments)))
               (progn (incf nr-skips) (pop timeline))))

    (loop while (and timeline (> (- rhythm-end max-deviation) (car timeline)))
          do (progn (incf nr-skips) (pop timeline)))

    (/ 1 (+ nr-ornaments nr-skips 1))))



(defun heur-min-skips-and-ornaments (layer-nr timeline tempo max-deviation factor)

  (let ((max-deviation-sec (/ (* 240 max-deviation) tempo)))
    (list
     #'(lambda (indexx x)
         (if (= layer-nr (get-layer-nr x))
           (let* ((this-voice-nr (get-voice-nr x))
                  (start-this-cell (1+ (get-stop-time this-voice-nr layer-nr (1- indexx))))
                  (stop-this-cell (1- (+ start-this-cell (get-variabledur x))))
                  (this-glogal-cell (om::om* (om::om+ start-this-cell (get-local-onset x))
                                             (append (get-pauses x) '(1))))
                  (onsets-in-rhythm (remove-pauses2 (append
                                                     (om::om* (om::om+ 1 (get-one-rhythmlayer this-voice-nr layer-nr (1- indexx)))
                                                              (get-one-layer-pauseflags this-voice-nr layer-nr (1- indexx)))
                                                     this-glogal-cell)
                                                    ))
                  ;as in hierarchy-rule - see Rules.lisp
                  (onsets-sec-in-rhythm (om::om/ (om::om* 240 (om::om- onsets-in-rhythm 1)) tempo))
                  (stop-time (/ (* 240 stop-this-cell) tempo)))

             (* factor (heur-check-max-in-seq onsets-sec-in-rhythm timeline max-deviation-sec stop-time)))
           0)))
    ))



(om::defmethod! RC::hr-quant_ornaments ((layer-nr integer)
                                        (timeline list)
                                        (tempo number)
                                        (max-deviation ratio)
                                        &optional (factor 1))


  :initvals '(1 nil  60 1/32 1)
  :indoc '("layernr" "seconds" "bpm (quarter note)" "ratio" "number")
  :doc "This heuristic rules should be used together with r-quant.

hr-quant_ornments will try to get the smallest number of ornaments and
skips possible. OBS: this rule has to be connected to the heuristic-rules
input (see the manual).

The engine will always prefere a small weight. The weight from this
rule is higher the more ornaments and skips that are found in the sequence.

The weight = 1/(nr of ornaments + nr of skips)

<factor> will be multiplied to the weight to allow the output to
be balanced to other heuristic rule.

Pauses are not supported. A pause at the beginning of a cell will
not be allowed (only as a skip). Pauses inside cells are supported."
  :icon 352


  (if (not factor) (setf factor 1))
  (heur-min-skips-and-ornaments layer-nr timeline tempo max-deviation factor))


;;;;;;;;

(defun heur-check-deviation (rytmseq timeline max-deviation rhythm-end)

  (let ((averagre-deviation 0))
    (loop while (and rytmseq timeline)
          do (if (<= (- (car rytmseq) max-deviation) (car timeline))
               (if (>= (+ (car rytmseq) max-deviation) (car timeline))
                 (progn (setf averagre-deviation
                              (/ (+ (abs (- (car rytmseq) (car timeline)))
                                    averagre-deviation)
                                 2))
                        (pop rytmseq) (pop timeline))
                 (pop rytmseq))
               (pop timeline)))

    (/ 1 (1+ (* 10 averagre-deviation)))
    ))


(defun heur-min-deviation (layer-nr timeline tempo max-deviation factor)

  (let ((max-deviation-sec (/ (* 240 max-deviation) tempo)))
    (list
     #'(lambda (indexx x)
         (if (= layer-nr (get-layer-nr x))
           (let* ((this-voice-nr (get-voice-nr x))
                  (start-this-cell (1+ (get-stop-time this-voice-nr layer-nr (1- indexx))))
                  (stop-this-cell (1- (+ start-this-cell (get-variabledur x))))
                  (this-glogal-cell (om::om* (om::om+ start-this-cell (get-local-onset x))
                                             (append (get-pauses x) '(1))))
                  (onsets-in-rhythm (remove-pauses2 (append
                                                     (om::om* (om::om+ 1 (get-one-rhythmlayer this-voice-nr layer-nr (1- indexx)))
                                                              (get-one-layer-pauseflags this-voice-nr layer-nr (1- indexx)))
                                                     this-glogal-cell)
                                                    ))
                  ;as in hierarchy-rule - see Rules.lisp
                  (onsets-sec-in-rhythm (om::om/ (om::om* 240 (om::om- onsets-in-rhythm 1)) tempo))
                  (stop-time (/ (* 240 stop-this-cell) tempo)))

             (* factor (heur-check-deviation onsets-sec-in-rhythm timeline max-deviation-sec stop-time)))
           0)))
    ))


(om::defmethod! RC::hr-quant_dev ((layer-nr integer)
                                  (timeline list)
                                  (tempo number)
                                  (max-deviation ratio)
                                  &optional (factor 1))


  :initvals '(1 nil  60 1/32 1)
  :indoc '("layernr" "seconds" "bpm (quarter note)" "ratio" "number")
  :doc "This heuristic rules should be used together with r-quant.

hr-quant_dev will try to minimize deviations from the timeline.
OBS: this rule has to be connected to the heuristic-rules
input (see the manual).

An exact match will give the weight 1.

Deviations will give the weight 1/(1 + deviation), where deviation
is measured in 1/10 seconds.

<factor> will be multiplied to the weight to allow the output to
be balanced to other heuristic rule.

Example: 0.1 sec deviation will give the weight 0.5, 0.3 sec deviation will give the weight 0.25.
"
  :icon 352


  (if (not factor) (setf factor 1))
  (heur-min-deviation layer-nr timeline tempo max-deviation factor)
  (print factor))


(om::defmethod! RC::hr-quant_dev2 ((layer-nr integer)
                                   (timeline list)
                                   (tempo number)
                                   (max-deviation ratio)
                                   (factor number))


  :initvals '(1 nil  60 1/32 1)
  :indoc '("layernr" "seconds" "bpm (quarter note)" "ratio" "number")
  :doc "Heuristic rule to minimize deviations from the timeline.
OBS: this rule has to be connected to the heuristic-rules
input (see the manual).

An exact match will give the weight 1.

Deviations will give the weight 1/(1 + deviation), where deviation
is measured in 1/10 seconds.

<factor> will be multiplied to the weight to allow the output to
be balanced to other heuristic rule.

Example: 0.1 sec deviation will give the weight 0.5, 0.3 sec deviation will give the weight 0.25.
"
  :icon 352



  (heur-min-deviation layer-nr timeline tempo max-deviation factor))
