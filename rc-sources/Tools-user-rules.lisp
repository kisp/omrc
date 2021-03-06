;****************************
;Rhythm Constraints library version 1.0 by rjan Sandred, IRCAM 1999
;
;Update version 1.3 19/8 2002 (Stockholm)
;
;Updated function in this document:
;  get-this-cell-from-svar, get-variable-in-this-layer-at-last-index, get-variable-in-this-layer-before-last-index,
;  get-variable-in-this-layer-two-before-last-index, get-all-variables-in-this-layer, get-all-variables-in-any-layer,
;  RC::get-cell-other-layer, RC::get-cell-any-layer, RC::get-rhythm-other-layer, RC::get-rhythm-any-layer,
;  RC::get-cell-at-time
;

(in-package RC)

(defun get-this-cell-from-svar (x)
      (if (typep x 'rhythmcell)
        (get-rhythmcell x)
        (get-timesign x)))

(defun get-variable-in-this-layer-at-last-index (indexx x)
  (let ((voice-nr (get-voice-nr x))
        (layer-nr (get-layer-nr x)))
      (if (typep x 'rhythmcell)
        (om::om* (om::x->dx (get-one-rhythmcell voice-nr layer-nr (1- indexx)))
                 (get-one-rhythmcells-pauses voice-nr layer-nr (1- indexx)))
        (get-one-timesign voice-nr layer-nr (1- indexx)))))


(defun get-variable-in-this-layer-before-last-index (indexx x)
  (let ((voice-nr (get-voice-nr x))
        (layer-nr (get-layer-nr x)))
      (if (typep x 'rhythmcell)
        (let ((abs-time-rhythm (get-rhythmcell-before-last-abs-time
                                voice-nr layer-nr (1- indexx))))
          (if abs-time-rhythm
            (om::om* (om::x->dx abs-time-rhythm)
                     (get-rhythmcell-pausflags-before-last voice-nr layer-nr (1- indexx)))
            nil))
        (get-timesign-before-last voice-nr layer-nr (1- indexx)))))



(defun get-variable-in-this-layer-two-before-last-index (indexx x)
  (let ((voice-nr (get-voice-nr x))
        (layer-nr (get-layer-nr x)))
      (if (typep x 'rhythmcell)
        (let ((abs-time-rhythm (get-rhythmcell-two-before-last-abs-time
                                voice-nr layer-nr (1- indexx))))
          (if abs-time-rhythm
            (om::om* (om::x->dx abs-time-rhythm)
                     (get-rhythmcell-pausflags-two-before-last voice-nr layer-nr (1- indexx)))
            nil))
        (get-timesign-two-before-last voice-nr layer-nr (1- indexx)))))


(defun get-all-variables-in-this-layer (indexx x)
  (let ((voice-nr (get-voice-nr x))
        (layer-nr (get-layer-nr x)))

      (if (typep x 'rhythmcell)
        (om::om* (get-all-rhythmcells-in-layer voice-nr layer-nr (1- indexx))
                 (get-all-rhythmcell-pauseflags-in-layer voice-nr layer-nr (1- indexx)))
        (get-one-measurelayer voice-nr layer-nr (1- indexx)))))

(defun get-all-variables-in-any-layer (indexx voice-nr layer-nr)

      (if (= layer-nr 0)
        (get-one-measurelayer voice-nr layer-nr (1- indexx))
        (om::om* (get-all-rhythmcells-in-layer voice-nr layer-nr (1- indexx))
                 (get-all-rhythmcell-pauseflags-in-layer voice-nr layer-nr (1- indexx)))))



;*********************
;om function
(om::defmethod! RC::get-this-cell ((x t))
   :initvals '(nil)
   :indoc '("search-var")
   :doc "Get the content of the current instantiated variable
(i.e. the current rhythm cell or time signature).

<x> is the current variable. It should be connected to the second input
inside a user rule patch.
--------------------------
Hmta innehllet i den aktuella instancierade variabeln (d.v.s. den
aktuella rytmcellen eller taktartssignaturen).

<x> r den aktuella variabeln. Den ska ansutas till den andra ingngen
inuti en patch fr en anvndardefinierad regel.
"
   :icon 367

   (get-this-cell-from-svar x)
   )


(om::defmethod! RC::get-last-cell ((indexx integer)
                                   (x t))
   :initvals '(nil nil)
   :indoc '("index" "search-var")
   :doc "Get the content of the instantiated variable in the same layer as the current one, but before the current one.

Get the content of the instantiated variable in the same layer as
the current one, but before the current one (i.e. a rhythm cell or
time signature).

<x> is the current variable. It should be connected to the second
input inside a user rule patch.
<indexx> is the index for the current variable (index is used internal
by the search engine). It should be connected to the first input inside
a user rule patch.
--------------------------
Hmta innehllet i den instancierade variabeln i samma skikt som den
aktuella instancierade variabeln, men fre denna (d.v.s. en rytmcell
eller taktartssignatur).

<x> r den aktuella variabeln. Den ska ansutas till den andra ingngen
inuti en patch fr en anvndardefinierad regel.
<indexx> r index fr den aktuella variabeln (index anvnds internt av
skmotorn). Den ska ansutas till den frsta ingngen inuti en patch fr
en anvndardefinierad regel.
"
   :icon 367

   (get-variable-in-this-layer-at-last-index indexx x)
   )


(om::defmethod! RC::get-cell-before-last ((indexx integer)
                                          (x t))
   :initvals '(nil nil)
   :indoc '("index" "search-var")
   :doc "Get the content of the instantiated variable in the same layer as the current one, but two before the current one.

Get the content of the instantiated variable in the same layer as the
current one, but two before the current one (i.e. a rhythm cell or
time signature).

<x> is the current variable. It should be connected to the second
input inside a user rule patch.
<indexx> is the index for the current variable (index is used internal
by the search engine). It should be connected to the first input inside
a user rule patch.
--------------------------
Hmta innehllet i den instancierade variabeln i samma skikt som den
aktuella instancierade variabeln, men tv fre denna (d.v.s. en rytmcell
eller taktartssignatur).

<x> r den aktuella variabeln. Den ska ansutas till den andra ingngen
inuti en patch fr en anvndardefinierad regel.
<indexx> r index fr den aktuella variabeln (index anvnds internt av
skmotorn). Den ska ansutas till den frsta ingngen inuti en patch fr
en anvndardefinierad regel.
"
   :icon 367

   (get-variable-in-this-layer-before-last-index indexx x)
   )


(om::defmethod! RC::get-cell-two-before-last ((indexx integer)
                                              (x t))
   :initvals '(nil nil)
   :indoc '("index" "search-var")
   :doc "Get the content of the instantiated variable in the same layer as the current one, but three before the current one.

Get the content of the instantiated variable in the same layer as the
current one, but three before the current one (i.e. a rhythm cell or
time signature).

<x> is the current variable. It should be connected to the second input
inside a user rule patch.
<indexx> is the index for the current variable (index is used internal
by the search engine). It should be connected to the first input inside
a user rule patch.
--------------------------
Hmta innehllet i den instancierade variabeln i samma skikt som den
aktuella instancierade variabeln, men tre fre denna (d.v.s. en rytmcell
eller taktartssignatur).

<x> r den aktuella variabeln. Den ska ansutas till den andra ingngen
inuti en patch fr en anvndardefinierad regel.
<indexx> r index fr den aktuella variabeln (index anvnds internt av
skmotorn). Den ska ansutas till den frsta ingngen inuti en patch fr
en anvndardefinierad regel.
"
   :icon 367

   (get-variable-in-this-layer-two-before-last-index indexx x)
   )


(om::defmethod! RC::get-all-cells ((indexx integer)
                                   (x t))
   :initvals '(nil nil)
   :indoc '("index" "search-var")
   :doc "Get the content of all instantiated variable in the same layer as the current one
(i.e. a list of all rhythm cells or time signatures). The current instantiated
variable is not included.

<x> is the current variable. It should be connected to the second input
inside a user rule patch.
<indexx> is the index for the current variable (index is used internal
by the search engine). It should be connected to the first input inside
a user rule patch.
--------------------------
Hmta innehllet av alla instancierade variabler i samma skikt som den
aktuella instancierade variabeln (d.v.s. en lista med alla rytmceller
eller taktartssignaturer). Den aktuella instacierade variabeln r inte
inklurderad.

<x> r den aktuella variabeln. Den ska ansutas till den andra ingngen
inuti en patch fr en anvndardefinierad regel.
<indexx> r index fr den aktuella variabeln (index anvnds internt av
skmotorn). Den ska ansutas till den frsta ingngen inuti en patch fr
en anvndardefinierad regel.
"
   :icon 367

   (get-all-variables-in-this-layer indexx x)
   )


(om::defmethod! RC::pause? ((x t))
   :initvals '(nil)
   :indoc '("search-var")
   :doc "Gives true if the current instaciated variable is a rhythm cell, and if the
cell contains longer total duration of pauses than notes.

<x> is the current variable. It should be connected to the second
input inside a user rule patch.
--------------------------
Ger sannt om den aktuella instancierade variabeln r en rytmcell, och om dess lngd till
strsta delen bestr av paus.

<x> r den aktuella variabeln. Den ska ansutas till den andra ingngen
inuti en patch fr en anvndardefinierad regel.
"
   :icon 367

   (and (typep x 'rhythmcell)
        (< (apply '+ (get-rhythmcell x))
           0)))


(om::defmethod! RC::rhythmcell? ((x t))
   :initvals '(nil)
   :indoc '("search-var")
   :doc "Gives true if the current instaciated variable is a rhythm cell.

<x> is the current variable. It should be connected to the second
input inside a user rule patch.
--------------------------
Ger sannt om den aktuella instancierade variabeln r en rytmcell.

<x> r den aktuella variabeln. Den ska ansutas till den andra ingngen
inuti en patch fr en anvndardefinierad regel.
"
   :icon 367

   (typep x 'rhythmcell)
   )


(om::defmethod! RC::timesign? ((x t))
   :initvals '(nil)
   :indoc '("search-var")
   :doc "Gives true if the current instaciated variable is a time signature.

<x> is the current variable. It should be connected to the second
input inside a user rule patch.
--------------------------
Ger sannt om den aktuella instancierade variabeln r en taktart.

<x> r den aktuella variabeln. Den ska ansutas till den andra ingngen
inuti en patch fr en anvndardefinierad regel.
"
   :icon 367

   (typep x 'timesign)
   )


(om::defmethod! RC::get-layernumber ((x t))
   :initvals '(nil)
   :indoc '("search-var")
   :doc "Get the layer number for the current instantiated variable.

<x> is the current variable. It should be connected to the second
input inside a user rule patch.
--------------------------
Hmta numret fr skiktet fr den aktuella instancierade variabeln.

<x> r den aktuella variabeln. Den ska ansutas till den andra ingngen
inuti en patch fr en anvndardefinierad regel.
"
   :icon 367

   (get-layer-nr x)
   )


(om::defmethod! RC::get-this-cell-dur ((x t))
   :initvals '(nil)
   :indoc '("search-var")
   :doc "Get the length of the current instantiated variable
(i.e. the length of the current rhythm cell or measure).

<x> is the current variable. It should be connected to the second
input inside a user rule patch.
--------------------------
Hmta lngden fr den aktuella instancierade variabeln (d.v.s.
lngden fr den aktuella rytmcellen eller takten).

<x> r den aktuella variabeln. Den ska ansutas till den andra
ingngen inuti en patch fr en anvndardefinierad regel.
"
   :icon 367

   (get-variabledur x)
   )


(om::defmethod! RC::test-equal ((ev1 t) (ev2 t))
   :initvals '(nil nil)
   :indoc '("event" "event")
   :doc "Gives true if two cells (rhythm cells or time signatures,also numbers or lists) are identical.

<ev1> is one of the events to compare.
<ev2> is the other.
--------------------------
Ger sannt om tv celler (rytmceller eller taktartssignaturer,ven siffror eller listor) r identiska.

<ev1> r en av hndelserna att jmfra.
<ev2> r den andra.
"
   :icon 380

   (equal ev1 ev2)
   )


(om::defmethod! RC::test-not-equal ((ev1 t) (ev2 t))
   :initvals '(nil nil)
   :indoc '("event" "event")
   :doc "Gives true if two cells (rhythm cells or time signatures,also numbers or lists) are different from each other.

<ev1> is one of the events to compare.
<ev2> is the other.
--------------------------
Ger sannt om tv celler (rytmceller eller taktartssignaturer,ven siffror eller listor) r olika.

<ev1> r en av hndelserna att jmfra.
<ev2> r den andra.
"
   :icon 381

   (not (equal ev1 ev2))
   )


(om::defmethod! RC::get-time ((indexx integer)
                              (x t))
   :initvals '(nil nil)
   :indoc '("index" "search-var")
   :doc "Get the start time for the current instantiated variable.
The time will be given as a ratio, equivalent to the total note
duration from the start of the sequence.

<x> is the current variable. It should be connected to the second
input inside a user rule patch.
<indexx> is the index for the current variable (index is used internal
by the search engine). It should be connected to the first input inside
a user rule patch.
--------------------------
Hmta starttiden fr den aktuella instancierade variabeln.
Tiden ges som ett brk, motsvarande det totala notlngdsvrdet frn
sekvensens start.

<x> r den aktuella variabeln. Den ska ansutas till den andra ingngen
inuti en patch fr en anvndardefinierad regel.
<indexx> r index fr den aktuella variabeln (index anvnds internt av
skmotorn). Den ska ansutas till den frsta ingngen inuti en patch fr
en anvndardefinierad regel.
"
   :icon 367

   (get-stop-time (get-voice-nr x) (get-layer-nr x) (1- indexx))
   );stop time because start time is wrong on last index


(om::defmethod! RC::get-cell-other-layer ((indexx integer)
                                          (x t)
                                          (layer integer))
   :initvals '(nil nil nil)
   :indoc '("index" "search-var" "nr")
   :doc "Get the content of the instantiated variable in another layer, but in the same voice, as the current instantiated variable, that exist at the starting point for the current instantiated variable.

Get the content of the instantiated variable in another layer,
but in the same voice, as the current instantiated variable, that
exist at the starting point for the current instantiated variable
(i.e. a rhythm cell or a time signature). If no variable is
instantiated yet, nil will be returned.

<x> is the current variable. It should be connected to the second
input inside a user rule patch.
<indexx> is the index for the current variable (index is used internal
by the search engine). It should be connected to the first input
inside a user rule patch.
<layer> is the layer number for the layer where the variable exist.
--------------------------
Hmta innehllet i den instancierade variabeln i ett annat skikt men
i samma stmma som den aktuella instancierade variabeln, som finns p
den aktuella instancierade variabelns starttid (d.v.s. en rytmcell
eller taktartssignatur).

<x> r den aktuella variabeln. Den ska ansutas till den andra ingngen
inuti en patch fr en anvndardefinierad regel.
<indexx> r index fr den aktuella variabeln (index anvnds internt
av skmotorn). Den ska ansutas till den frsta ingngen inuti en patch
fr en anvndardefinierad regel.
<layer> r numret fr det skikt dr variabeln finns.
"
   :icon 367

   (let ((timepoint (get-stop-time (get-voice-nr x) (get-layer-nr x) (1- indexx))))
     (if (= layer 0)
       (get-timesign-at-timepoint (get-voice-nr x) layer timepoint (1- indexx))
       (om::om* (om::x->dx (get-rhythmcell-at-timepoint (get-voice-nr x) layer timepoint (1- indexx)))
                (get-rhythmcells-pauses-at-timepoint (get-voice-nr x) layer timepoint (1- indexx))))
     ))


(om::defmethod! RC::get-cell-any-layer ((indexx integer)
                                        (x t)
                                        (layer integer)
                                        (voice integer))
   :initvals '(nil nil nil nil)
   :indoc '("index" "search-var" "nr" "nr")
   :doc "Get the content of an instantiated variable in any layer and voice that exist at the starting point for the current instantiated variable.

Get the content of an instantiated variable in any layer and
voice that exists at the starting point for the current instantiated
variable (i.e. a rhythm cell or a time signature). If no variable is
instantiated yet, nil will be returned.

<x> is the current variable. It should be connected to the
second input inside a user rule patch.
<indexx> is the index for the current variable (index is used
internal by the search engine). It should be connected to the
first input inside a user rule patch.
<layer> is the layer number for the layer where the variable exist.
<voice> is the voice number for the voice where the variable exist.
--------------------------
Hmta innehllet i den instancierade variabeln i vilket skikt
och vilken stmma som helst som finns p den aktuella instancierade
variabelns starttid (d.v.s. en rytmcell eller taktartssignatur).

<x> r den aktuella variabeln. Den ska ansutas till den andra
ingngen inuti en patch fr en anvndardefinierad regel.
<indexx> r index fr den aktuella variabeln (index anvnds
internt av skmotorn). Den ska ansutas till den frsta ingngen
inuti en patch fr en anvndardefinierad regel.
<layer> r numret fr det skikt dr variabeln finns.
<voice> r numret fr det stmma dr variabeln finns.
"
   :icon 367

   (let ((timepoint (get-stop-time (get-voice-nr x) (get-layer-nr x) (1- indexx))))
     (if (= layer 0)
       (get-timesign-at-timepoint voice layer timepoint (1- indexx))
       (om::om* (om::x->dx (get-rhythmcell-at-timepoint voice layer timepoint (1- indexx)))
                (get-rhythmcells-pauses-at-timepoint voice layer timepoint (1- indexx)))))
   )


(om::defmethod! RC::get-rhythm-other-layer ((indexx integer)
                                            (x t)
                                            (layer integer))
  :initvals '(nil nil nil)
  :indoc '("index" "search-var" "nr")
  :doc "Get the rhythm in another layer, but in the same voice, as the current instantiated variable, that occur within the timeframe for the current instanced variable.

Get the rhythm in another layer, but in the same voice, as the current
instantiated variable, that occur within the timeframe for the current
instanced variable. This is not similar to a rhythm cell, since parts
of one or several cells might occur within the timeframe. If no rhythm
exist yet, nil will be returned.

<x> is the current variable. It should be connected to the second input
inside a user rule patch.
<indexx> is the index for the current variable (index is used internal
by the search engine). It should be connected to the first input inside
a user rule patch.
<layer> is the layer number for the layer where the rhythm is.
--------------------------
Hmta rytmen i ett annat skikt men i samma stmma som den aktuella
instancierade variabeln, som frekommer under den aktuella instancierade
variabelns tidsrymd. Detta r inte samma sak som en rytmcell, eftersom
delar av en eller flera celler kan frekomma inom tidsrymden. Om ingen
rytm finns nnu returneras nil.

<x> r den aktuella variabeln. Den ska ansutas till den andra ingngen
inuti en patch fr en anvndardefinierad regel.
<indexx> r index fr den aktuella variabeln (index anvnds internt av
skmotorn). Den ska ansutas till den frsta ingngen inuti en patch fr
en anvndardefinierad regel.
<layer> r numret fr det skikt dr rytmen finns.
"
  :icon 367

  (let* ((starttime (get-stop-time (get-voice-nr x) (get-layer-nr x) (1- indexx)))
         (endtime (+ starttime (get-variabledur x))))
    (if (= layer 0)
      nil
      (om::om* (om::x->dx (get-rhythm-within-timepoints (get-voice-nr x) layer (1- indexx) starttime endtime))
               (get-pauseflags-within-timepoints (get-voice-nr x) layer (1- indexx) starttime endtime)))))


(om::defmethod! RC::get-rhythm-any-layer ((indexx integer)
                                          (x t)
                                          (layer integer)
                                          (voice integer))
  :initvals '(nil nil nil nil)
  :indoc '("index" "search-var" "nr" "nr")
  :doc "Get a rhythm that occurs within the timeframe for the current instanced variable in any layer and voice.
This is not similar to a rhythm cell, since parts of one or several
cells might occur within the timeframe. If no rhythm exist yet, nil
will be returned.

<x> is the current variable. It should be connected to the second input
inside a user rule patch.
<indexx> is the index for the current variable (index is used internal
by the search engine). It should be connected to the first input inside
a user rule patch.
<layer> is the layer number for the layer where the rhythm is.
<voice> is the voice number for the voice where the rhythm is.
--------------------------
Hmta en rytm som frekommer under den aktuella instancierade variabelns
tidsrymd i vilket skikt och vilken stmma som helst. Detta r inte samma
sak som en rytmcell, eftersom delar av en eller flera celler kan frekomma
inom tidsrymden. Om ingen rytm finns nnu returneras nil.

<x> r den aktuella variabeln. Den ska ansutas till den andra ingngen
inuti en patch fr en anvndardefinierad regel.
<indexx> r index fr den aktuella variabeln (index anvnds internt av
skmotorn). Den ska ansutas till den frsta ingngen inuti en patch fr
en anvndardefinierad regel.
<layer> r numret fr det skikt dr rytmen finns.
<voice> r numret fr det stmma dr rytmen finns.
"
  :icon 367

  (let* ((starttime (get-stop-time (get-voice-nr x) (get-layer-nr x) (1- indexx)))
         (endtime (+ starttime (get-variabledur x))))
    (if (= layer 0)
      nil
      (om::om* (om::x->dx (get-rhythm-within-timepoints voice layer (1- indexx) starttime endtime))
               (get-pauseflags-within-timepoints voice layer (1- indexx) starttime endtime)))))


(om::defmethod! RC::get-all-cells-any-layer ((indexx integer)
                                             (layer integer)
                                             (voice integer))
  :initvals '(nil nil nil)
  :indoc '("index" "nr" "nr")
  :doc "Get the content of all instantiated variables in any layer and voice.
NO DIFFERENCE WILL BE MADE BETWEEN PAUSES AND NOTES IN
RHYTHM CELLS.

<indexx> is the index for the current variable (index is used
internal by the search engine). It should be connected to the
first input inside a user rule patch.
<layer> is the layer number for the layer.
<voice> is the voice number for the voice where the layer is.
--------------------------
Hmta innehllet i alla instancierade variabler i vilket skikt
och vilken stmma som helst. INGEN SKILLNAD GRS MELLAN
PAUSER OCH NOTER I RYTMCELLER.

<indexx> r index fr den aktuella variabeln (index anvnds
internt av skmotorn). Den ska ansutas till den frsta ingngen
inuti en patch fr en anvndardefinierad regel.
<layer> r numret fr skiktet.
<voice> r numret fr det stmma dr skiktet finns.
"
  :icon 367

  (get-all-variables-in-any-layer indexx voice layer))



(om::defmethod! RC::get-cell-at-time ((indexx integer)
                                      (timepoint number)
                                      (layer integer)
                                      (voice integer))
   :initvals '(nil 0 nil nil)
   :indoc '("index" "time" "nr" "nr")
   :doc "Tool to get a cell in any voice/layer at a given timepoint."
   :icon 367

   (if (< timepoint 0)
     nil
     (if (= layer 0)
       (get-timesign-at-timepoint voice layer timepoint (1- indexx))
       (om::om* (om::x->dx (get-rhythmcell-at-timepoint voice layer timepoint (1- indexx)))
                (get-rhythmcells-pauses-at-timepoint voice layer timepoint (1- indexx)))))
   )

;***********bakat kompatibilitet
(om::defmethod! RC::get-cell-duration ((x t))
   :initvals '(nil)
   :indoc '("search-var")
   :doc "Tool to get a what layer a searchvariable is a member of."
   :icon 367

   (get-variabledur x)
   )
