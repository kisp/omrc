;****************************
;Rhythm Constraints library version 1.0 by rjan Sandred, IRCAM 1999
;
;Updated to version 1.3 19/8 2002 (Stockholm)
;
;Updated functions in this document:
;  get-pauses-fixlayer, decode-rhythm-fixlayers, get-one-layer-from-last-sol-pmc->simple
;(Functions with Csolver might not work - probably not). Only Pmc is supported in this update.
;
;


(in-package RC)
;-------------------------------
;DECODEING
;decode a solution with or without timesign. Low layer-nr first in output.
(defun decode-solution-simple (situation-solution)
  ;output: list of
  ;           list of
  ;                list of layers in a voice (starting from low to high layer nr, timesign has to be first)
  ;           list of '((voice-nr) layer-numbers)
  ; ex. ((((1/4 1/4)(1/2 1/4))((1/8 1/8)(1/4 1/4))) ((1) 1 2) ((2) 1 2))

  (if situation-solution
    (let ((cell-list
           (loop for i from 0 to (1- (length (car situation-solution)))
                 collect (if (typep (nth i (car situation-solution)) 'rc::rhythmcell)
                           (get-rhythmcell (nth i (car situation-solution)))
                           (list (get-timesign (nth i (car situation-solution)))))))
          (layernr-list
           (loop for i from 0 to (1- (length (car situation-solution)))
                 collect (get-layer-nr (nth i (car situation-solution)))))
          (voicenr-list
           (loop for i from 0 to (1- (length (car situation-solution)))
                 collect (get-voice-nr (nth i (car situation-solution)))))
          (voice/layer nil)
          this-layer)

      (append
       (list
        ;Decode and sort the rhythms/timesigns
        (loop for voice-nr from (apply 'min voicenr-list) to (apply 'max voicenr-list)
              collect (remove nil
                              (loop for layer-nr from (apply 'min layernr-list) to (apply 'max layernr-list)
                                    collect (progn (setf this-layer
                                                         (remove nil
                                                                 (apply 'append
                                                                        (mapcar
                                                                         #'(lambda (layer voice cell) (if (and (= layer layer-nr)
                                                                                                               (= voice voice-nr))
                                                                                                        cell nil))
                                                                         layernr-list voicenr-list cell-list))))
                                                   (if this-layer
                                                     (progn (setf voice/layer (append voice/layer (list (list voice-nr layer-nr))))
                                                            this-layer)
                                                     nil))
                                    ))))

       ;Format the code for what the layers above belongs to.
       (remove nil (loop for voice-nr from 0 to (1- *max-numberof-voices*)
                         collect (progn (setf this-layer (remove nil (mapcar #'(lambda (voice/layer-code)
                                                                                  (if (= voice-nr (car voice/layer-code))
                                                                                    (cadr voice/layer-code) nil))
                                                                              voice/layer)))
                                         (if this-layer
                                           (cons (list voice-nr) this-layer)
                                           nil))))
       ))

    ;If the input is nil
    (progn (print "I can not decode the solution since it is empty!") nil)
    ))



(defun decode-Csolver->poly (situation-solution tempo)
  (let* ((sol (decode-solution-simple situation-solution))
         (all-layerseqs (car sol))
         (maxlengthlist '(0))
         maxlength
         time-signs
         rhythmlayers)

    (loop for voice from 0 to (1- (length all-layerseqs))
          collect (make-instance 'om::poly
                    :voices
                    (let ((layerseq (nth voice all-layerseqs)))

                      ;Check if first sublist is a list of timesign
                      (if (typep (caar layerseq) 'list)
                        (progn (setf time-signs (car layerseq)) (setf rhythmlayers (cdr layerseq)))
                        ;if not, check max length of rhythmlayers and bulid a default timesign list.
                        (progn (dolist (rlayer layerseq)
                                 (setf maxlengthlist (append (list (apply '+ (mapcar 'abs rlayer))) maxlengthlist)))
                               (if (= (rem (apply 'max maxlengthlist) 1) 0)
                                 (setf maxlength (truncate (apply 'max maxlengthlist)))
                                 (setf maxlength (1+ (truncate (apply 'max maxlengthlist)))))
                               (setf time-signs (make-list maxlength :initial-element '(4 4)))
                               (setf rhythmlayers layerseq)))
                      (if (not rhythmlayers) (setf rhythmlayers '((-4))))
                      (if (not time-signs) (setf time-signs '((4 4))))
                      ;build rhythm trees and voices for each layer

                      (loop for x from 0 to (1- (length rhythmlayers))
                            collect  (make-instance 'om::voice
                                       :tempo tempo
                                       :tree (simple->tree  (nth x rhythmlayers) time-signs)
                                       :legato 99
                                       :chords (make-list (length (nth x rhythmlayers))
                                                          :initial-element
                                                          (make-instance 'om::chord
                                                            :LChan (list (1+ x))))
                                       )))))))

(defun decode-Csolver->voices (situation-solution tempo)
  (let* ((sol (decode-solution-simple situation-solution))
         (all-layerseqs (car sol))
         (maxlengthlist '(0))
         maxlength
         time-signs
         rhythmlayers)

    (apply 'append
           (loop for voice from 0 to (1- (length all-layerseqs))
                 collect (let ((layerseq (nth voice all-layerseqs)))

                           ;Check if first sublist is a list of timesign
                           (if (typep (caar layerseq) 'list)
                             (progn (setf time-signs (car layerseq)) (setf rhythmlayers (cdr layerseq)))
                             ;if not, check max length of rhythmlayers and bulid a default timesign list.
                             (progn (dolist (rlayer layerseq)
                                      (setf maxlengthlist (append (list (apply '+ (mapcar 'abs rlayer))) maxlengthlist)))
                                    (if (= (rem (apply 'max maxlengthlist) 1) 0)
                                      (setf maxlength (truncate (apply 'max maxlengthlist)))
                                      (setf maxlength (1+ (truncate (apply 'max maxlengthlist)))))
                                    (setf time-signs (make-list maxlength :initial-element '(4 4)))
                                    (setf rhythmlayers layerseq)))
                           (if (not rhythmlayers) (setf rhythmlayers '((-4))))
                           (if (not time-signs) (setf time-signs '((4 4))))
                           ;build rhythm trees and voices for each layer

                           (loop for x from 0 to (1- (length rhythmlayers))
                                 collect  (make-instance 'om::voice
                                            :tempo tempo
                                            :tree (simple->tree  (nth x rhythmlayers) time-signs)
                                            :legato 99
                                            :chords (make-list (length (nth x rhythmlayers))
                                                               :initial-element
                                                               (make-instance 'om::chord
                                                                 :LChan (list (+ (1+ x) (* voice (length rhythmlayers))))))
                                            )))))))


;;************** DECODE FIXLAYERS IN SOLUTION
(defun get-rhythm-fixlayer (voice layer-nr)
  (butlast (get-one-rhythmcell voice layer-nr 0)))


(defun get-pauses-fixlayer (voice layer-nr)
  (butlast (get-one-rhythmcells-pauses voice layer-nr 0)))

(defun detect-fix-layer (voice)
  (loop for layer-nr from 0 to 4
        collect (if (/= (aref pointers-vector voice layer-nr 1 0) 0)
                  layer-nr)))


(defun decode-rhythm-fixlayers (voice)
  ;output: list of
  ;           list of layers (starting form low to high layer nr)
  ;           list of layer numbers
  (let ((fixlayer-nrs (remove 0 (remove nil (detect-fix-layer voice))))
        (answer nil))
    (dolist (layer-nr fixlayer-nrs)
      (setf answer (append answer (list (om::om* (get-pauses-fixlayer voice layer-nr)
                                                 (om::x->dx (get-rhythm-fixlayer voice layer-nr)))))))
    (if answer
      (list answer fixlayer-nrs)
      nil)))


(defun decode-measure-fixlayers (voice)
  ;output: list of
  ;           list of layers (starting form low to high layer nr)
  ;           list of layer numbers
  (let ((fixlayer-nrs (remove nil (detect-fix-layer voice)))
        (pointer -1))
    (if (member 0 fixlayer-nrs)
      (loop while (aref part-timegrid-vector voice 1 (1+ pointer))
            collect (progn
                      (setf pointer (1+ pointer))
                      (aref part-timegrid-vector voice 1 pointer)))
      nil)))


(defun decode-rhythm-fixlayers->poly (voice tempo)
  (let ((fixlayer-seqs (car (decode-rhythm-fixlayers voice)))
        (maxlength 1)
        maxlengthlist
        time-signs)

    (if fixlayer-seqs
      (progn (dolist (rlayer fixlayer-seqs)
               (setf maxlengthlist (append (list (apply '+ (mapcar 'abs rlayer))) maxlengthlist)))
             (if (= (rem (apply 'max maxlengthlist) 1) 0)
               (setf maxlength (truncate (apply 'max maxlengthlist)))
               (setf maxlength (1+ (truncate (apply 'max maxlengthlist))))))
      (setf fixlayer-seqs '((-4))))
    (setf time-signs (make-list maxlength :initial-element '(4 4)))

     (loop for x from 0 to (1- (length fixlayer-seqs))
           collect  (make-instance 'om::voice
                      :tempo tempo
                      :tree (simple->tree  (nth x fixlayer-seqs) time-signs)
                      :legato 99
                      :chords (make-list (length (nth x fixlayer-seqs))
                                         :initial-element
                                         (make-instance 'om::chord
                                           :LChan (list (1+ x))))
                      ))
     ))


(defun decode-fixlayers->simple (voice)
  (append (list (decode-measure-fixlayers voice))
          (car (decode-rhythm-fixlayers voice))))


(defun decode-fixlayers->voices (voice tempo)
  (let ((fixlayers-rhythm (car (decode-rhythm-fixlayers voice)))
        (fixlayers-measures (decode-measure-fixlayers voice))
        (maxlength 1)
        maxlengthlist
        time-signs)

    (if fixlayers-rhythm
      (progn (dolist (rlayer fixlayers-rhythm)
               (setf maxlengthlist (append (list (apply '+ (mapcar 'abs rlayer))) maxlengthlist)))
             (if (= (rem (apply 'max maxlengthlist) 1) 0)
               (setf maxlength (truncate (apply 'max maxlengthlist)))
               (setf maxlength (1+ (truncate (apply 'max maxlengthlist))))))
      (setf fixlayers-rhythm '((-4))))
    (if fixlayers-measures
      (setf time-signs fixlayers-measures)
      (setf time-signs (make-list maxlength :initial-element '(4 4))))

     (loop for x from 0 to (1- (length fixlayers-rhythm))
           collect  (make-instance 'om::voice
                      :tempo tempo
                      :tree (simple->tree  (nth x fixlayers-rhythm) time-signs)
                      :legato 99
                      :chords (make-list (length (nth x fixlayers-rhythm))
                                         :initial-element
                                         (make-instance 'om::chord
                                           :LChan (list (1+ x))))
                      ))
     ))


(defun decode-fixlayers->poly (voice tempo)
  (make-instance 'om::poly
    :voices (decode-fixlayers->voices voice tempo)))

;------------------
(defun decode-sol&fixlayers->simple (situation-solution)
  (let* ((sol (decode-solution-simple situation-solution))
         (all-layerseqs (car sol))
         (voice/layer-codes (cdr sol))
         (voicenr-list (mapcar 'caar voice/layer-codes))
         (pointer-to-list 0)
         (answer nil))

    (dolist (voice-nr voicenr-list)
      (let* ((fixlayers-rhythm (decode-rhythm-fixlayers voice-nr))
             (fixlayers-measures (decode-measure-fixlayers voice-nr))
             (layernr-fix-measures -1)
             (fixlayers-voice-list (car fixlayers-rhythm))
             (layernr-fix (cadr fixlayers-rhythm))
             (layerseqs (nth pointer-to-list all-layerseqs))
             (layernr-sol (cdr (nth pointer-to-list voice/layer-codes))))
        (incf pointer-to-list)

        ;If a precalculated measure layer exist, replace layer zero with this.
        ;Only give the number of measures needed (filtered out in the loop below).
        (if fixlayers-measures
          (let ((measures-start (om::dx->x 0 (mapcar #'(lambda (time-sign) (apply '/ time-sign))
                                                     fixlayers-measures)))
                (maxlength 0)
                (maxlengthlist '(0)))
            (dolist (rlayer layerseqs)
              (setf maxlengthlist (append (list (apply '+ (mapcar 'abs rlayer))) maxlengthlist)))
            (if (= (rem (apply 'max maxlengthlist) 1) 0)
               (setf maxlength (truncate (apply 'max maxlengthlist)))
               (setf maxlength (1+ (truncate (apply 'max maxlengthlist)))))
            (let ((nr-in-list 0))
              (setf fixlayers-measures
                    (loop until (or (> (nth nr-in-list measures-start) maxlength)
                                    (>= nr-in-list (length fixlayers-measures)))
                          collect (progn (incf nr-in-list)
                                         (nth (1- nr-in-list) fixlayers-measures))))
              )
            (setf layernr-fix-measures 0)))

        ;Sort fixlayers and layers from solution to the correct position
        (setf answer
              (append answer (list
                              (remove nil
                                      (loop for x from 0 to *max-numberof-layers*
                                            collect (cond ((= x layernr-fix-measures)
                                                           fixlayers-measures)
                                                          ((member x layernr-fix)
                                                           (pop fixlayers-voice-list))
                                                          ((member x layernr-sol)
                                                           (pop layerseqs))
                                                          ))))
                      ))))
    answer
    ))



(defun decode-sol&fixlayers->poly (situation-solution tempo)
  (let ((all-layerseqs (decode-sol&fixlayers->simple situation-solution))
        (maxlengthlist '(0))
        maxlength
        time-signs
        rhythmlayers)

    (loop for voice from 0 to (1- (length all-layerseqs))
          collect (make-instance 'om::poly
                    :voices
                    (let ((layerseq (nth voice all-layerseqs)))

                      ;Check if first sublist is a list of timesign
                      (if (typep (caar layerseq) 'list)
                        (progn (setf time-signs (car layerseq)) (setf rhythmlayers (cdr layerseq)))
                        ;if not, check max length of rhythmlayers and bulid a default timesign list.
                        (progn (dolist (rlayer layerseq)
                                 (setf maxlengthlist (append (list (apply '+ (mapcar 'abs rlayer))) maxlengthlist)))
                               (if (= (rem (apply 'max maxlengthlist) 1) 0)
                                 (setf maxlength (truncate (apply 'max maxlengthlist)))
                                 (setf maxlength (1+ (truncate (apply 'max maxlengthlist)))))
                               (setf time-signs (make-list maxlength :initial-element '(4 4)))
                               (setf rhythmlayers layerseq)))
                      (if (not rhythmlayers) (setf rhythmlayers '((-4))))
                      (if (not time-signs) (setf time-signs '((4 4))))
                      ;build rhythm trees and voices for each layer
                      (loop for x from 0 to (1- (length rhythmlayers))
                            collect  (make-instance 'om::voice
                                       :tempo tempo
                                       :tree (simple->tree  (nth x rhythmlayers) time-signs)
                                       :legato 99
                                       :chords (make-list (length (nth x rhythmlayers))
                                                          :initial-element
                                                          (make-instance 'om::chord
                                                            :LChan (list (1+ x))))
                                       )))))))


(defun decode-sol&fixlayers->voices (situation-solution tempo)
  (let ((all-layerseqs (decode-sol&fixlayers->simple situation-solution))
        (maxlengthlist '(0))
        maxlength
        time-signs
        rhythmlayers)

    (apply 'append
           (loop for voice from 0 to (1- (length all-layerseqs))
                 collect (let ((layerseq (nth voice all-layerseqs)))

                           ;Check if first sublist is a list of timesign
                           (if (typep (caar layerseq) 'list)
                             (progn (setf time-signs (car layerseq)) (setf rhythmlayers (cdr layerseq)))
                             ;if not, check max length of rhythmlayers and bulid a default timesign list.
                             (progn (dolist (rlayer layerseq)
                                      (setf maxlengthlist (append (list (apply '+ (mapcar 'abs rlayer))) maxlengthlist)))
                                    (if (= (rem (apply 'max maxlengthlist) 1) 0)
                                      (setf maxlength (truncate (apply 'max maxlengthlist)))
                                      (setf maxlength (1+ (truncate (apply 'max maxlengthlist)))))
                                    (setf time-signs (make-list maxlength :initial-element '(4 4)))
                                    (setf rhythmlayers layerseq)))
                           (if (not rhythmlayers) (setf rhythmlayers '((-4))))
                           (if (not time-signs) (setf time-signs '((4 4))))
                           ;build rhythm trees and voices for each layer
                           (loop for x from 0 to (1- (length rhythmlayers))
                                 collect  (make-instance 'om::voice
                                            :tempo tempo
                                            :tree (simple->tree  (nth x rhythmlayers) time-signs)
                                            :legato 99
                                            :chords (make-list (length (nth x rhythmlayers))
                                                               :initial-element
                                                               (make-instance 'om::chord
                                                                 :LChan (list (+ (1+ x) (* voice (length rhythmlayers))))))
                                            )))))))



(defun get-one-layer-from-last-sol-pmc->simple (voice-nr layer-nr)
  (if (= layer-nr 0)
    (if (member 0 (detect-fix-layer voice-nr))
      (decode-measure-fixlayers voice-nr) ; presetlayers will output 20 extra measures at the end.
      (if (check-if-measure-domain? voice-nr)
        (get-one-measurelayer voice-nr 0 (get-last-solindex-pmc))
        nil))
    (if (member layer-nr (remove nil (detect-fix-layer 0)))
      ;if it is a predefined layer, take away the added extra endpoint (see "lock-one-predefined-layer")
      (butlast  (om::om* (om::x->dx (get-one-rhythmlayer voice-nr layer-nr (get-last-solindex-pmc)))
                         (get-one-layer-pauseflags voice-nr layer-nr (get-last-solindex-pmc))))
      (om::om* (om::x->dx (get-one-rhythmlayer voice-nr layer-nr (get-last-solindex-pmc)))
               (get-one-layer-pauseflags voice-nr layer-nr (get-last-solindex-pmc))))))



(defun get-one-rlayer-from-last-sol-pmc->voice (voice-nr layer-nr tempo)
  (make-instance 'om::voice
    :tree (simpleformat->tree  (get-one-layer-from-last-sol-pmc->simple voice-nr layer-nr)
                               (if (get-one-layer-from-last-sol-pmc->simple voice-nr 0)
                                 (get-one-layer-from-last-sol-pmc->simple voice-nr 0)
                                 '(4 4)))
    :legato 99
    :tempo tempo
    ))

(defun get-one-mlayer-from-last-sol-pmc->voice (voice-nr tempo)
  (make-instance 'om::voice
    :tree (simple->tree  '(-100) (get-one-layer-from-last-sol-pmc->simple voice-nr 0))
    :legato 99
    :tempo tempo
    ))

;---------------------
;Create om boxes


(om::defmethod! RC::decode-engine ((sol list)
                                   (tempo number)
                                   &optional (output 'poly)(presets? 'yes))

   :initvals '('() 60 'poly 'yes)
   :indoc '("from engine" "bpm" "format" "yes/no")
   :menuins '((2 (("poly-list" 'poly) ("voice-list" 'voice) ("simple" 'simple)))
             (3 (("yes" 'yes) ("no" 'no))))
   :doc "Decode the solution from the pmc (also works for Csolver).

<sol> is the solution from pmc (or Csolver).
<tempo> is the tempo that the notation window (\Òvoice\Ó or \Òpoly\Ó) will use.
<output> is the format for the output of this box. You can either get a list
of poly-objects (each one representing one voice in the solution), or a list
of voice-objects (each one representing one rhythm layer in the solution),
or a list in \Òsimple format\Ó (with a sublist for each voice containing sublists for
the time signatures and every rhythm layer). Choose with the help of the
pop-up menu.
<presets?> You can chose whether to include the preset layers in the output.
Choose with the help of the pop-up menu.

If the voice-list format is chosen, each voice will address a new MIDI channel.
If the poly-list is chosen, each voice in a poly-box will address a new MIDI
channel. If no time signatures exist in the solution, 4//4 is used as default.
--------------------------
Avkoda lsningen frn pmc (fungerar ven med Csolver).

<sol> r lsningen frn pmc (eller Csolver).
<tempo> r det tempo som notationsfnstret (\Òvoice\Ó eller \Òpoly\Ó) kommer att
anvnda.
<output> r det format man fr resultatet i. Man kan antingen f en
lista med poly-objekt (dr varje objekt representerar en stmma i lsningen),
eller en lista med voice-objekt (dr varje objekt representerar ett rytmskikt
i lsningen), eller en list i \Òsimple format\Ó (med en sub-lista fr varje stmma,
bestende av sub-listor fr taktartssignaturer och varje rytm skikt). Vlj
med hjlp at pop-up menyn.
<presets?> Man kan vlja att inkludera frdefinierade skikt eller inte i
resultatet. Vlj med hjlp av pop-up menyn.

Om voice-list formatet r valt kommer varje ny voice adressera en ny MIDI kanal.
Om poly-list formatet r valt kommer varje ny voice i ett poly-objekt att
adressera en ny MIDI kanal. Om inga taktarter existerar i lsningen anvnds
4//4-takt.
"

    :icon 366

    (case presets?
      ((yes) (case output
               ((poly) (decode-sol&fixlayers->poly sol tempo))
               ((voice) (decode-sol&fixlayers->voices sol tempo))
               ((simple) (decode-sol&fixlayers->simple sol))))
      ((no) (case output
              ((poly) (decode-Csolver->poly sol tempo))
              ((voice) (decode-Csolver->voices sol tempo))
              ((simple) (car (decode-solution-simple sol))))))
    )



(om::defmethod! RC::view-presets ((voice integer)
                                  &optional (tempo 60) (output 'simple))

  :initvals '(0 60 'simple)
  :indoc '("voice nr" "bpm" "format")
  :menuins '((2 (("simple" 'simple) ("voices" 'voice) ("poly" 'poly))))
  :doc "View all predefined layers in one voice.

<voice> is the number for the voice to be viewed.
<tempo> is the tempo that the notation window (\Òvoice\Ó or \Òpoly\Ó) will use.
<output> is the format for the output of this box. You can either get a
poly-object, or a list of voice-objects (each one representing one rhythm
layer in the solution), or a list in \Òsimple format\Ó  (a list of sublists for
the time signatures and every rhythm layer). Choose with the help of the
pop-up menu.

If the voices format is chosen, each voice will address a new MIDI channel.
If the poly format is chosen, each voice in the poly-box will address a
new MIDI channel. If no time signature is present, 4//4 is used as default.
--------------------------
Visa alla frdefinierade skikt i en stmma.

<voice> r numret fr stmman som ska visas.
<tempo> r det tempo som notationsfnstret (\Òvoice\Ó eller \Òpoly\Ó) kommer
att anvnda.
<output> r det format som man fr resultatet i. Man kan antingen f ett
poly-objekt, eller en lista med voice-objekt (dr varje objekt representerar
ett rytmskikt i lsningen), eller en lista i \Òsimple format\Ó (en lista med sub-listor
fr taktartssignaturer och varje rytm skikt). Vlj med hjlp at pop-up menyn.

Om voices formatet r valt kommer varje ny voice adressera en ny MIDI kanal.
Om poly formatet r valt kommer varje ny voice i poly-objektet att adressera
en ny MIDI kanal. Om inga taktarter existerar i lsningen anvnds 4//4-takt.
"

  :icon 364

  (case output
      ((poly) (decode-fixlayers->poly voice tempo))
      ((voice) (decode-fixlayers->voices voice tempo))
      ((simple) (remove nil (decode-fixlayers->simple voice))))
  )


(om::defmethod! RC::layer-in-solution ((voice integer)
                                       (layer integer)
                                       &optional (tempo 60) (output 'simple))

   :initvals '(0 1 60 'simple)
   :indoc '("voicenr" "layernr" "bpm" "format")
   :menuins '((3 (("simple" 'simple) ("voice" 'voice))))
   :doc "Decode one layer from the last found solution (pmc).

<voice> is the voice number for the layer.
<layer> is the layer number for the layer (0 will only give the time signatures).
<tempo> is the tempo that the notation window (\Òvoice\Ó) will use.
<output> is the format for the output of this box. You can either get the
output as a voice-object or a list in \Òsimple format\Ó  (a list of all duration
values or time signatures). Choose with the help of the pop-up menu.
--------------------------
Avkoda ett skikt frn den senaste funna lsningen (pmc).

<voice> r numret fr stmman.
<layer> r numret fr skiktet (vlj 0 fr att bara f taktarterna).
<tempo> r det tempo som notationsfnstret (\Òvoice\Ó) kommer att anvnda.
<output> r det format som man fr resultatet i. Man kan antingen f
resultatet som ett voice-objekt eller som en lista i \Òsimple format\Ó (en lista
med alla notvrden eller taktartssignaturer). Vlj med hjlp at pop-up menyn.
"
   :icon 377

   (case output
      ((voice) (if (= layer 0)
                  (get-one-mlayer-from-last-sol-pmc->voice voice tempo)
                  (get-one-rlayer-from-last-sol-pmc->voice voice layer tempo)))
      ((simple) (get-one-layer-from-last-sol-pmc->simple voice layer)))
   )



;************CSLOVER COMPABILITY

(defun get-one-layer-from-last-sol-csolv->simple (voice-nr layer-nr)
  (if (= layer-nr 0)
    (get-used-mlayer voice-nr (get-last-solindex-csolver))
    (if (member layer-nr (remove nil (detect-fix-layer 0)))
      ;if it is a predefined layer, take away the added extra endpoint (see "lock-one-predefined-layer")
      (butlast (om::x->dx (get-one-rhythmlayer voice-nr layer-nr (get-last-solindex-csolver))))
      (om::x->dx (get-one-rhythmlayer voice-nr layer-nr (get-last-solindex-csolver))))))


(defun get-one-rlayer-from-last-sol-csolv->voice (voice-nr layer-nr tempo)
  (make-instance 'om::voice
    :tree (simple->tree  (get-one-layer-from-last-sol-csolv->simple voice-nr layer-nr) (get-one-layer-from-last-sol-csolv->simple voice-nr 0))
    :legato 99
    :tempo tempo
    ))


(defun get-one-mlayer-from-last-sol-csolv->voice (voice-nr tempo)
  (make-instance 'om::voice
    :tree (simple->tree  '(-100) (get-one-layer-from-last-sol-csolv->simple voice-nr 0))
    :legato 99
    :tempo tempo
    ))

(om::defmethod! RC::layer-in-sol/csolv ((voice integer)
                                        (layer integer)
                                        &optional (tempo 60) (output 'simple))

   :initvals '(0 1 60 'simple)
   :indoc '("voicenr" "layernr" "tempo" "format")
   :menuins '((3 (("simple" 'simple) ("voice" 'voice))))
   :doc "Decode one layer from the last found solution (Csolver).

<voice> is the voice number for the layer.
<layer> is the layer number for the layer (0 will only give the time signatures).
<tempo> is the tempo that the notation window (\Òvoice\Ó) will use.
<output> is the format for the output of this box. You can either get the
output as a voice-object or a list in \Òsimple format\Ó  (a list of all duration
values or time signatures). Choose with the help of the pop-up menu.
--------------------------
Avkoda ett skikt frn den senaste funna lsningen (Csolver).

<voice> r numret fr stmman.
<layer> r numret fr skiktet (vlj 0 fr att bara f taktarterna).
<tempo> r det tempo som notationsfnstret (\Òvoice\Ó) kommer att anvnda.
<output> r det format som man fr resultatet i. Man kan antingen f
resultatet som ett voice-objekt eller som en lista i \Òsimple format\Ó (en lista
med alla notvrden eller taktartssignaturer). Vlj med hjlp at pop-up menyn.
"
   :icon 374


   (case output
      ((voice) (if (= layer 0)
                  (get-one-mlayer-from-last-sol-csolv->voice voice tempo)
                  (get-one-rlayer-from-last-sol-csolv->voice voice layer tempo)))
      ((simple) (get-one-layer-from-last-sol-csolv->simple voice layer)))
   )

(om::defmethod! RC::partial-sol/csolv->poly ((tempo number))

   :initvals '(60)
   :indoc '("tempo")
   :doc "Use to view a partial solution in a poly box.

Every new voice address next MIDI channel"
   :icon 374


   (decode-sol&fixlayers->poly
    (list (remove nil (cl-user::partial-solution)))
    tempo)
   )

;************FOR BACK COMPATIBILITY


(om::defmethod! RC::engine->simple/no-preset  ((sol list))

   :indoc '("from engine")
    :doc "Decode result from the CSolver or the pmc. Do not include pre calculated layers."
    :icon 366


    (car (decode-solution-simple sol))
    )

(om::defmethod! RC::engine->poly/no-preset ((sol list)
                                            (tempo number))

   :initvals '('() 60 )
   :indoc '("from engine" "tempo")
    :doc "Decode result from the CSolver or the pmc, and builds a score in a poly object.
Do not include pre calculated layers. If no timesignatures are
present, 4/4 will be given as default.

Every new voice address next MIDI channel"
    :icon 366


    (decode-Csolver->poly sol tempo)
    )

(om::defmethod! RC::engine->simple ((sol list))

   :initvals '('() 60 )
   :indoc '("from engine")
    :doc "Decode result from the CSolver or the pmc. Pre calculated layers are included."
    :icon 366


    (decode-sol&fixlayers->simple sol)
    )

(om::defmethod! RC::engine->voices ((sol list)
                                    (tempo number))

   :initvals '('() 60 )
   :indoc '("from engine" "tempo")
    :doc "Decode result from the CSolver or the pmc, and build a list of voice objects.
Pre calculated layers are included. If no timesignatures
are present, 4/4 will be given as default. Voices are not separated,
but comes in order.

Every new voice address next MIDI channel"
    :icon 366


    (decode-sol&fixlayers->voices sol tempo)
    )

(om::defmethod! RC::engine->poly ((sol list)
                                  (tempo number))

   :initvals '('() 60 )
   :indoc '("from engine" "tempo")
    :doc "Decode result from the CSolver or the pmc, and builds a score in a poly object.
Pre calculated layers are included. If no timesignatures
are present, 4/4 will be given as default.

Every new voice address next MIDI channel"
    :icon 366


    (decode-sol&fixlayers->poly sol tempo)
    )


(om::defmethod! RC::layer-in-sol/pmc->simple ((voice integer)
                                              (layer integer))

   :initvals '(0 1)
   :indoc '("voicenr" "layernr")
   :doc "outdated, use newer version"
   :icon 377


   (print "outdated, use newer version")
   )

(om::defmethod! RC::layer-in-sol/csolv->simple ((voice integer)
                                                (layer integer))

   :initvals '(0 1)
   :indoc '("voicenr" "layernr")
   :doc "outdated, use newer version"
   :icon 374


   (print "outdated, use newer version")
   )

(om::defmethod! RC::decode-engine/no-presets ((sol list)
                                              (tempo number)
                                              &optional (output 'poly))

   :initvals '('() 60 'poly)
   :indoc '("from engine" "bpm" "format")
   :menuins '((2 (("poly-list" 'poly) ("voice-list" 'voice) ("simple" 'simple))))
   :doc "Decode the solution from the pmc (also works for Csolver) excluding predefined layers.

<sol> is the solution from pmc (or Csolver).
<tempo> is the tempo that the notation window (\Òvoice\Ó or \Òpoly\Ó) will use.
<output> is the format for the output of this box. You can either get a list
of poly-objects (each one representing one voice in the solution), or a list
of voice-objetcs (each one representing one rhythm layer in the solution),
or in simple format (with a sublist for each voice containing sublists for
the time signatures and every rhythm layer). Choose with the help of the
pop-up menu.

If the voice-list format is chosen, each voice will address a new MIDI channel.
If the poly-list is chosen, each voice in a poly-box will address a new MIDI
channel. If no time signatures exist in the solution, 4/4 is used as default.
--------------------------
Avkoda lsningen frn pmc (fungerar ven med Csolver) utan frdefinierade skikt.

<sol> r lsningen frn pmc (eller Csolver).
<tempo> r det tempo som notationsfnstret (\Òvoice\Ó eller \Òpoly\Ó) kommer att
anvnda.
<output> r det format man fr resultatet i. Man kan antingen f en
lista med poly-objekt (dr varje objekt representerar en stmma i lsningen),
eller en lista med voice-objekt (dr varje objekt representerar ett rytmskikt
i lsningen), eller i \Òsimple format\Ó (med en sub-lista fr varje stmma,
bestende av sub-listor fr taktartssignaturer och varje rytm skikt). Vlj
med hjlp at pop-up menyn.

Om voice-list formatet r valt kommer varje ny voice adressera en ny MIDI kanal.
Om poly-list formatet r valt kommer varje ny voice i ett poly-objekt att
adressera en ny MIDI kanal. Om inga taktarter existerar i lsningen anvnds
4/4-takt."

    :icon 366

    (case output
      ((poly) (decode-Csolver->poly sol tempo))
      ((voice) (decode-Csolver->voices sol tempo))
      ((simple) (car (decode-solution-simple sol))))
    )
