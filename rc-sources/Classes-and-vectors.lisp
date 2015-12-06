;****************************
;Rhythm Constraints library version 1.0 by rjan Sandred, IRCAM 1999
;
;
;Update to 1.3 19/8 2002 (Stockholm)
;Updated vector in this document:
;  part-sol-vector (has now a dimension for flags to indicate pauses)
;
;Updated functions in this document:
;  put-this-rhythmcell, get-one-rhythmcell, get-one-rhythmcells-pauses, get-start-time, get-stop-time,
;  get-one-rhythmlayer, get-one-layer-pauseflags, get-rhythmcell-before-last-abs-time, get-rhythmcell-pausflags-before-last,
;  get-rhythmcell-two-before-last-abs-time, get-rhythmcell-pausflags-two-before-last, get-all-rhythmcells-in-layer,
;  get-all-rhythmcell-pauseflags-in-layer, get-all-cell-startpoints-in-one-layer, get-rhythmcells-pauses-at-timepoint,
;  get-pauseflags-within-timepoints
;

(in-package RC)
(defparameter *max-numberof-layers* 4) ;This does not include measure layer.
(defparameter *max-numberof-voices* 7)
;this is also limited by the functions create-domain-for-all-voices & collect-voices-rules

(defparameter *max-numberof-rhythmvalues* 2400)
(defparameter *max-size-timesigngrid* 8160)
(defparameter *max-numberof-variables* 800)


;The rules use three vectors to store temporary solutions between checking variables
;The purpose is to increase speed by re-useing calculated variables.
;The first vector (part-sol-vector) store maximum four layers per voice were every element
;is a onset time from the beginning of the sequence.

;The second vectoreis a list of (absolute time) timesignatures and grid points. It is used by the
;rule r-beat-subdiv.

;The third vector (pointers-vector) is a vector of pointers to the other vectors.
;You can access the information by giving the index (the same as you get in a rule
;in Situation) to a point in the solution, and what layer you want to look at.

;Example: You want to know what rhythm-cell the Csolver chose for voice 1, layer 1
;at index 25. (aref pointers-vector 1 1 0 25) will give you a pointer to use to
;look in the vector part-sol-vector. (aref pointers-vector 1 1 1 25) will give you
;the length of the same cell. If (setf my-pointer (aref pointers-vector 1 1 0 25)),
;then (part-sol-vector 1 1 my-pointer) will give you the first onset time for the
;rhythmcell you are asking for.

;Since the Csolver only picks a cell for one layer at one index, the other layers won't get new cells
;at this point. The function step-fwd-pointers compensates for this, and is called from a rule that
;is generated by the rules->pmc (see the function rc::call-rule-update-variables-all-voices-pmc). This rule updates
;the vectors. Without this rule, the vectors will not be correct.


;OBS OBS OBS In pointers-vector rhythm layer 1 - 4 are called 1 - 4, but in part-sol-vector they are called 0 - 3!
;VECTORS and access to vectors
; (part-sol-vector voice layer-nr onsettimes rhythm/pause-flags))
(defvar part-sol-vector (make-array (list *max-numberof-voices*
                                          *max-numberof-layers*
                                          *max-numberof-rhythmvalues*
                                          2)
            :initial-element 0))

; (part-timegrid-vector voice onsetforgrid/timesign grid)
(defvar part-timegrid-vector (make-array (list *max-numberof-voices* 2
                                               *max-size-timesigngrid*)
            :initial-element 0))

; (pointers-vector voice layer-nr pointerstart/pointerend index) - layer-nr is here 0 = mlayer, 1 - 4 = rlayer
(defvar pointers-vector (make-array (list *max-numberof-voices*
                                          (1+ *max-numberof-layers*) 2
                                          *max-numberof-variables*)
            :initial-element 0))

(defun put-this-rhythmcell (rhythm-cell-abs rhythm-cell-pause-flags voice layer-nr index)
  (let ((local-pointer (aref pointers-vector voice layer-nr 1 index))
        (flagpointer -1))
    (setf (aref pointers-vector voice layer-nr 0 (1+ index)) local-pointer)
    (decf layer-nr)
    (dolist (one-onset (cdr rhythm-cell-abs))
      (incf local-pointer)
      (incf flagpointer)
      (setf (aref part-sol-vector voice layer-nr local-pointer 0) one-onset)
      (setf (aref part-sol-vector voice layer-nr (1- local-pointer) 1) (nth flagpointer rhythm-cell-pause-flags)))
    (setf (aref pointers-vector voice (1+ layer-nr) 1 (1+ index)) local-pointer)
    ))

(defun get-one-rhythmcell (voice layer-nr index)    ;;without pauses
  (let ((start-pointer (aref pointers-vector voice layer-nr 0 (1+ index)))
        (end-pointer (aref pointers-vector voice layer-nr 1 (1+ index))))
    (decf layer-nr)
    (loop for local-pointer from start-pointer to end-pointer
          collect (aref part-sol-vector voice layer-nr local-pointer 0))))

(defun get-one-rhythmcells-pauses (voice layer-nr index)
  (let ((start-pointer (aref pointers-vector voice layer-nr 0 (1+ index)))
        (end-pointer (1- (aref pointers-vector voice layer-nr 1 (1+ index)))))
    (decf layer-nr)
    (loop for local-pointer from start-pointer to end-pointer
          collect (aref part-sol-vector voice layer-nr local-pointer 1))))

(defun step-fwd-pointers (voice layer-nr index)
  (setf (aref pointers-vector voice layer-nr 0 (1+ index))
        (aref pointers-vector voice layer-nr 0 index))
  (setf (aref pointers-vector voice layer-nr 1 (1+ index))
        (aref pointers-vector voice layer-nr 1 index)))

(defun get-start-time (voice layer-nr index)
  (let ((start-pointer (aref pointers-vector voice layer-nr 0 (1+ index))))
    (if (= layer-nr 0)
      (aref part-timegrid-vector voice 0 start-pointer)
      (aref part-sol-vector voice (1- layer-nr) start-pointer 0))))

(defun get-stop-time (voice layer-nr index)
  (let ((stop-pointer (aref pointers-vector voice layer-nr 1 (1+ index))))
    (if (= layer-nr 0)
      (aref part-timegrid-vector voice 0 stop-pointer)
      (aref part-sol-vector voice (1- layer-nr) stop-pointer 0))))

(defun get-one-rhythmlayer (voice layer-nr last-index)  ;;without pauses
  (let ((stop-pointer (aref pointers-vector voice layer-nr 1 (1+ last-index))))
    (decf layer-nr)
      (loop for local-pointer from 0 to stop-pointer
            collect (aref part-sol-vector voice layer-nr local-pointer 0))))

(defun get-one-layer-pauseflags (voice layer-nr last-index)
  (let ((stop-pointer (aref pointers-vector voice layer-nr 1 (1+ last-index))))
    (decf layer-nr)
      (loop for local-pointer from 0 to (1- stop-pointer)
            collect (aref part-sol-vector voice layer-nr local-pointer 1))))

(defun put-this-timesign (timesign local-abs-grid voice layer-nr index)
  (let ((local-pointer (aref pointers-vector voice layer-nr 1 index)))
    (setf (aref pointers-vector voice layer-nr 0 (1+ index)) local-pointer)
    (setf (aref part-timegrid-vector voice 1 local-pointer) timesign)
    (dolist (one-gridpoint (cdr local-abs-grid))
      (incf local-pointer)
      (setf (aref part-timegrid-vector voice 0 local-pointer) one-gridpoint))
    (setf (aref pointers-vector voice layer-nr 1 (1+ index)) local-pointer)))

; get-one-timesign not for predefined layers
(defun get-one-timesign (voice layer-nr index)
  (let ((start-pointer (aref pointers-vector voice layer-nr 0 (1+ index))))
        (if (apply '= (rc::get-one-layer-all-stop-pointers voice layer-nr index))
          nil   ;to avoid first ghoast value before any layer exist
          (aref part-timegrid-vector voice 1 start-pointer))))

(defun get-timegrid-layer (voice layer-nr last-index)
  (let ((stop-pointer (aref pointers-vector voice layer-nr 1 (1+ last-index))))
    (loop for local-pointer from 0 to stop-pointer
          collect (aref part-timegrid-vector voice 0 local-pointer))))

(defun get-one-layer-all-start-pointers (voice layer-nr this-index)
  (loop for step-index from 0 to (1+ this-index) ; 1+ because a new cell for an index is stored on index+1
        collect (aref pointers-vector voice layer-nr 0 step-index)))

(defun get-one-layer-all-stop-pointers (voice layer-nr this-index)
  (loop for step-index from 0 to (1+ this-index) ; 1+ because a new cell for an index is stored on index+1
        collect (aref pointers-vector voice layer-nr 1 step-index)))

;(defun get-one-measurelayer (voice layer-nr last-index)
;  (let ((pointer-list  (remove-duplicates
;                        (get-one-layer-all-start-pointers voice layer-nr last-index))))
;    (loop for nr-in-list from 0 to (1- (length pointer-list))
;          collect (aref part-timegrid-vector voice 1 (nth nr-in-list pointer-list)))))

(defun get-one-measurelayer (voice layer-nr last-index)
  (let ((pointer-list (remove-duplicates
                       (get-one-layer-all-start-pointers voice layer-nr last-index))))
    (if (apply '= (rc::get-one-layer-all-stop-pointers voice layer-nr last-index))
      nil   ;to avoid first ghoast value before any layer exist
      (loop for nr-in-list from 0 to (1- (length pointer-list))
            collect (aref rc::part-timegrid-vector voice 1 (nth nr-in-list pointer-list))))))
;OBS If a layer is locked, get-one-measurelayer will not give a correct answer!!!

;SPECIAL CASES FOR TOOLS-FOR-USER-RULES
(defun get-rhythmcell-before-last-abs-time (voice layer-nr last-index)
  (let* ((start-pointer-list  (remove-duplicates
                               (get-one-layer-all-start-pointers voice layer-nr last-index)))
         (end-pointer-list  (remove-duplicates
                             (get-one-layer-all-stop-pointers voice layer-nr last-index)))
         (start-pointer (second (reverse start-pointer-list)))
         (end-pointer (second (reverse end-pointer-list))))
    (decf layer-nr)
    (if (not start-pointer) (setf start-pointer 0))
    (if (or (not end-pointer) (= end-pointer 0))
      nil
      (loop for local-pointer from start-pointer to end-pointer
            collect (aref part-sol-vector voice layer-nr local-pointer 0)))))


(defun get-rhythmcell-pausflags-before-last (voice layer-nr last-index)
  (let* ((start-pointer-list  (remove-duplicates
                               (get-one-layer-all-start-pointers voice layer-nr last-index)))
         (end-pointer-list  (remove-duplicates
                             (get-one-layer-all-stop-pointers voice layer-nr last-index)))
         (start-pointer (second (reverse start-pointer-list)))
         (end-pointer (second (reverse end-pointer-list))))
    (decf layer-nr)
    (if (not start-pointer) (setf start-pointer 0))
    (if (or (not end-pointer) (= end-pointer 0))
      nil
      (loop for local-pointer from start-pointer to (1- end-pointer)
            collect (aref part-sol-vector voice layer-nr local-pointer 1)))))


(defun get-timesign-before-last (voice layer-nr last-index)
  (let* ((start-pointer-list  (remove-duplicates
                               (get-one-layer-all-start-pointers voice layer-nr last-index)))
         (start-pointer (second (reverse start-pointer-list)))
         )

    (if (apply '= (rc::get-one-layer-all-stop-pointers voice layer-nr last-index))
      nil
        (if (not start-pointer)
          nil
          (aref part-timegrid-vector voice 1 start-pointer)))
    ))

(defun get-rhythmcell-two-before-last-abs-time (voice layer-nr last-index)
  (let* ((start-pointer-list  (remove-duplicates
                               (get-one-layer-all-start-pointers voice layer-nr last-index)))
         (end-pointer-list  (remove-duplicates
                             (get-one-layer-all-stop-pointers voice layer-nr last-index)))
         (start-pointer (third (reverse start-pointer-list)))
         (end-pointer (third (reverse end-pointer-list))))
    (decf layer-nr)
    (if (not start-pointer) (setf start-pointer 0))
    (if (or (not end-pointer) (= end-pointer 0))
      nil
      (loop for local-pointer from start-pointer to end-pointer
            collect (aref part-sol-vector voice layer-nr local-pointer 0)))))


(defun get-rhythmcell-pausflags-two-before-last (voice layer-nr last-index)
  (let* ((start-pointer-list  (remove-duplicates
                               (get-one-layer-all-start-pointers voice layer-nr last-index)))
         (end-pointer-list  (remove-duplicates
                             (get-one-layer-all-stop-pointers voice layer-nr last-index)))
         (start-pointer (third (reverse start-pointer-list)))
         (end-pointer (third (reverse end-pointer-list))))
    (decf layer-nr)
    (if (not start-pointer) (setf start-pointer 0))
    (if (or (not end-pointer) (= end-pointer 0))
      nil
      (loop for local-pointer from start-pointer to (1- end-pointer)
            collect (aref part-sol-vector voice layer-nr local-pointer 1)))))


(defun get-timesign-two-before-last (voice layer-nr last-index)
  (let* ((start-pointer-list  (remove-duplicates
                               (get-one-layer-all-start-pointers voice layer-nr last-index)))
         (start-pointer (third (reverse start-pointer-list))))

    (if (apply '= (rc::get-one-layer-all-stop-pointers voice layer-nr last-index))
      nil
      (if (not start-pointer)
        nil
        (aref part-timegrid-vector voice 1 start-pointer)))
    ))


(defun get-all-rhythmcells-in-layer (voice layer-nr last-index)
  (let ((start-pointer-list  (remove-duplicates
                              (get-one-layer-all-start-pointers voice layer-nr last-index)))
        (end-pointer-list  (cdr (remove-duplicates
                                 (get-one-layer-all-stop-pointers voice layer-nr last-index)))))
    (decf layer-nr)
    (mapcar #'(lambda (startpointer endpointer)
                (om::x->dx
                 (loop for local-pointer from startpointer to endpointer
                       collect (aref part-sol-vector voice layer-nr local-pointer 0))))
            start-pointer-list end-pointer-list)))



(defun get-all-rhythmcell-pauseflags-in-layer (voice layer-nr last-index)
  (let ((start-pointer-list  (remove-duplicates
                              (get-one-layer-all-start-pointers voice layer-nr last-index)))
        (end-pointer-list  (cdr (remove-duplicates
                                 (get-one-layer-all-stop-pointers voice layer-nr last-index)))))
    (decf layer-nr)
    (mapcar #'(lambda (startpointer endpointer)
                 (loop for local-pointer from startpointer to (1- endpointer)
                       collect (aref part-sol-vector voice layer-nr local-pointer 1)))
            start-pointer-list end-pointer-list)))



(defun get-all-cell-startpoints-in-one-layer (voice layer-nr last-index)
  (let ((start-pointer-list  (remove-duplicates
                              (get-one-layer-all-start-pointers voice layer-nr last-index))))
    (decf layer-nr)
    (mapcar #'(lambda (startpointer) (aref part-sol-vector voice layer-nr startpointer 0))
            start-pointer-list)))


(defun get-rhythmcell-at-timepoint (voice layer-nr timepoint last-index)
  (let ((this-rlayer (get-one-rhythmlayer voice layer-nr last-index)))

    (if (< (car (reverse this-rlayer)) timepoint)
      nil
      (progn
        (let* ((pointer-to-timepoint (- (length this-rlayer)
                                        (position timepoint (reverse this-rlayer) :test '>=)
                                        1))
               (pointer-to-cellstarts (get-one-layer-all-start-pointers voice layer-nr last-index))
               (pointer-to-this-cellstart (find pointer-to-timepoint (reverse pointer-to-cellstarts) :test '>=))
               (index-for-cell (- (length pointer-to-cellstarts)
                                  (position pointer-to-this-cellstart (reverse pointer-to-cellstarts))
                                  2)))
          (get-one-rhythmcell voice layer-nr index-for-cell))))))


(defun get-rhythmcells-pauses-at-timepoint (voice layer-nr timepoint last-index)
  (let ((this-rlayer (get-one-rhythmlayer voice layer-nr last-index)))

    (if (< (car (reverse this-rlayer)) timepoint)
      nil
      (progn
        (let* ((pointer-to-timepoint (- (length this-rlayer)
                                        (position timepoint (reverse this-rlayer) :test '>=)
                                        1))
               (pointer-to-cellstarts (get-one-layer-all-start-pointers voice layer-nr last-index))
               (pointer-to-this-cellstart (find pointer-to-timepoint (reverse pointer-to-cellstarts) :test '>=))
               (index-for-cell (- (length pointer-to-cellstarts)
                                  (position pointer-to-this-cellstart (reverse pointer-to-cellstarts))
                                  2)))
          (get-one-rhythmcells-pauses voice layer-nr index-for-cell))))))



(defun get-timesign-at-timepoint (voice layer-nr timepoint last-index)
  (let ((this-mgridlayer (get-timegrid-layer voice layer-nr last-index)))

    (if (< (car (reverse this-mgridlayer)) timepoint)
      nil
      (progn
        (let* ((pointer-to-timepoint (- (length this-mgridlayer)
                                        (position timepoint (reverse this-mgridlayer) :test '>=)
                                        1))
               (pointer-to-cellstarts (get-one-layer-all-start-pointers voice layer-nr last-index))
               (pointer-to-this-timesign (find pointer-to-timepoint (reverse pointer-to-cellstarts) :test '>=)))

          (aref part-timegrid-vector voice 1 pointer-to-this-timesign))))))


(defun get-rhythm-within-timepoints (voice layer-nr last-index starttime endtime)
  (let ((this-rlayer (get-one-rhythmlayer voice layer-nr last-index)))
    (member starttime (reverse (member endtime (reverse this-rlayer) :test '>=)) :test '<=)))

(defun get-pauseflags-within-timepoints (voice layer-nr last-index starttime endtime)
  (let ((this-rlayer (get-one-rhythmlayer voice layer-nr last-index))
        (this-rlayer-pauseflags (get-one-layer-pauseflags voice layer-nr last-index)))
    (mapcar #'(lambda (value) (if (> value  0) 1 -1))
            (member (1+ starttime) (reverse (member (1+ endtime) (reverse (om::om* (om::om+ this-rlayer 1) this-rlayer-pauseflags))
                                               :test #'(lambda (x y) (>= x (abs y)))))
                    :test #'(lambda (x y) (<= x (abs y)))))))


;;;NEW functions to support new canon rule 3/9/02

(defun get-stretched-rhythm-within-timepoints (voice layer-nr last-index starttime endtime factor)
  (let ((this-rlayer (om::om* factor (get-one-rhythmlayer voice layer-nr last-index))))
    (member starttime (reverse (member endtime (reverse this-rlayer) :test '>=)) :test '<=)))

(defun get-stretched-pauseflags-within-timepoints (voice layer-nr last-index starttime endtime factor)
  (let ((this-rlayer (om::om* factor (get-one-rhythmlayer voice layer-nr last-index)))
        (this-rlayer-pauseflags (get-one-layer-pauseflags voice layer-nr last-index)))
    (mapcar #'(lambda (value) (if (> value  0) 1 -1))
            (member (1+ starttime) (reverse (member (1+ endtime) (reverse (om::om* (om::om+ this-rlayer 1) this-rlayer-pauseflags))
                                               :test #'(lambda (x y) (>= x (abs y)))))
                    :test #'(lambda (x y) (<= x (abs y)))))))

;-------------------------------
;classes for search variables
(defclass rc-voice ()
  ((voice-nr :type integer :initform nil :reader get-voice-nr :writer set-voice-nr)))
(defclass layer (rc-voice)
  ((layer :type integer :initform nil :reader get-layer-nr :writer set-layer-nr)))
(defclass variabledur (layer)
  ((variabledur :type ratio :initform 0 :reader get-variabledur :writer set-variabledur)))
(defclass rhythmcell (variabledur)
  ((rhythmcell :type list :initform '() :reader get-rhythmcell :writer set-rhythmcell)
   (local-onset :type list :initform '() :reader get-local-onset :writer set-local-onset)
   (pauses :type list :initform '() :reader get-pauses :writer set-pauses)))
(defclass timesign (variabledur)
  ((timesign :type list :initform '() :reader get-timesign :writer set-timesign)))