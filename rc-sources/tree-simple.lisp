;****************************
;Rhythm Constraints library version 1.0 by rjan Sandred, IRCAM 1999
;

;Use the function tree->simple. All other functions are called from this one.
;This function builds a sequence of rhythm values on the format (1/4 1/4 1/4 1/4) from
;a hierarchical rhythmtree.

; If a pause is "tied", this function will not give correct answer.
; However, pauses can't be tied!
; If the first value is tied (from "nothing"), this will give a floating point value in the result.

;used om functions: om*, flat
;
;UPDATE 1.3. Bug in function fuse-pauses fixed (will now never output floats).
;UPDATE 1.32. Bug in function unpack-tree fixed (will now work on lower hierarcical levels in the rhythmtree).


(in-package RC)

(defmethod sum-one-beat ((proportions list))
  (truncate (apply '+ (mapcar 'abs proportions))))


(defmethod sum-one-beat ((proportions number))
  1) ; 1 as a multiple to the value itself

(defmethod get-beat-dur ((subtree number))
  1) ; 1 as a multiple to the value itself

(defmethod get-beat-dur ((subtree list))
  (first subtree))

(defmethod unpack-tree ((sub-tree number))
  sub-tree)

(defmethod unpack-tree-level2 ((sub-tree number))
  sub-tree)

(defmethod unpack-tree-level3 ((sub-tree number))
  sub-tree)

(defmethod unpack-tree ((sub-tree list))
  (let* ((proportional-format (mapcar 'unpack-tree-level2 sub-tree))
         (beat-dur (mapcar 'get-beat-dur sub-tree))
         (prop-length-beat (mapcar 'sum-one-beat proportional-format))
         (multiples-for-list (mapcar #'(lambda (beat) (/ (apply 'lcm prop-length-beat) beat))
                                     prop-length-beat)))
    (om::flat (om::om* beat-dur (mapcar 'om::om* multiples-for-list proportional-format)))))

(defmethod unpack-tree-level2 ((sub-tree list))
  (let* ((proportional-format (mapcar 'unpack-tree-level3 (second sub-tree)))
         (prop-length-beat (mapcar 'sum-one-beat proportional-format))
         (multiples-for-list (mapcar #'(lambda (beat) (/ (apply 'lcm prop-length-beat) beat))
                                     prop-length-beat)))
    (om::flat  (mapcar 'om::om* multiples-for-list proportional-format))))

(defmethod unpack-tree-level3 ((sub-tree list))
  (let* ((proportional-format (mapcar 'unpack-tree-level2 (second sub-tree)))
         (beat-dur (first sub-tree))
         (prop-length-beat (mapcar 'sum-one-beat proportional-format))
         (multiples-for-list (mapcar #'(lambda (beat) (/ (apply 'lcm prop-length-beat) beat))
                                     prop-length-beat)))
    (om::flat (om::om* beat-dur (mapcar 'om::om* multiples-for-list proportional-format)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;tools for function below

(defun find-common-multiplier (multipliers)
  (apply 'lcm (mapcar #'(lambda (value) (/ 1 value)) multipliers)))

(defun make-multiplier-list (multipliers)
  (om::om* (find-common-multiplier multipliers) multipliers))


(defun fuse-ties (proportion-list)
  (let ((result nil))
    (loop while proportion-list
          do (progn (cond ((and result
                                (typep (car proportion-list) 'float)
                                (> (car proportion-list) 0))
                           (setf (car (last result)) (+ (car (last result)) (truncate (car proportion-list)))))
                          ((and result
                                (typep (car proportion-list) 'float)
                                (< (car proportion-list) 0)) ;to avoid error for "slured pauses"
                           (setf (car proportion-list) (truncate (car proportion-list)))
                           (setf result (append result (list (car proportion-list)))))
                          (t
                           (setf result (append result (list (car proportion-list))))))
                    (pop proportion-list)))
    result))

(defun fuse-pauses (proportion-list)
  (let ((result nil))
    (loop while proportion-list
          do (progn (if (and result
                             (< (first proportion-list) 0)
                             (< (car (last result)) 0))
                      (setf (car (last result)) (truncate (+ (car (last result)) (car proportion-list))))
                      (setf result (append result (list (car proportion-list)))))
                    (pop proportion-list)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun measures->simple (measures)
  (let* ((proportions (mapcar #'(lambda (measure)
                                  (unpack-tree (cadr measure)))
                              measures))
         (multipliers (mapcar #'(lambda (measure proportion) (/ (apply '/ (car measure))
                                                     (truncate (apply '+ (mapcar 'abs proportion)))))
                              measures proportions))
         (global-proportion-list (om::flat (om::om* proportions (make-multiplier-list multipliers))))
         (global-multiplier (/ (first multipliers) (first (make-multiplier-list multipliers)))))


    (om::om* (fuse-ties (fuse-pauses global-proportion-list)) global-multiplier)
    ))


;************************
;om function
(om::defmethod! RC::tree->simpleformat ((tree list))

  :initvals '('(2 (((4 4) (1 (1 (1 -2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1)))))
  :indoc '("tree")
  :doc "Convert a rhythm tree to a list of note values (ratios).

<tree> is a rhythm tree.

Pauses following each other will be fused to one (longer)
pause. Slured note values will be fused to one note value.
--------------------------
Konvertera ett rytmtrd till en lista av notvrden (brk).

<tree> r ett rytmtrd.

Pauser som fljer p varandra kommer att noteras sammanslagna
till en (lngre) paus. verbundna noter kommer att noteras
sammanslagna till ett notvrde.
"
  :icon 379

  (measures->simple (second tree))
  )


;older version. Will give wrong answer if pauses are "slured" (ex. (-1 -1.0))
;(defun fuse-ties (proportion-list)
;  (let ((result nil))
;    (loop while proportion-list
;          do (progn (if (and result
;                             (typep (car proportion-list) 'float))
;                      (setf (car (last result)) (+ (car (last result)) (truncate (car proportion-list))))
;                      (setf result (append result (list (car proportion-list)))))
;                    (pop proportion-list)))
;    result))
