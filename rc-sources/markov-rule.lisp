;;;;;;;;;;;;;;;;
;Markov simulation rule in OMRC 1.4


(in-package RC)


(defun format-weights-table (cells weights)
  (let ((scaled-weights (mapcar #'(lambda (one-row)
                                    (let ((factor (apply '+ one-row)))
                                      (mapcar #'(lambda (weight) (/ weight factor)) one-row)))
                                    weights))
        (cell-pairs (mapcar #'(lambda (cell1)
                                (mapcar #'(lambda (cell2) (list cell1 cell2)) cells))
                            cells)))
    (if (/= (length cells) (length weights))
      (print "Warning: number of cells and rows of weights do not correspond."))
    (let ((test (mapcar 'length weights)))
      (if (not (apply '= (append (list (length cells)) test)))
        (print "Warning: number of cells and columns of weights do not correspond.")))
    (mapcar 'list (apply 'append cell-pairs) (apply 'append scaled-weights))
        ))


(defun filter-weights-table (weights-table pair-start-value)
  (remove pair-start-value weights-table :test #'(lambda (test1 test2) (not (equal test1 (caar test2))))))


(defun format-pairs-from-sequence (cell-sequence)
  (butlast (mapcon #'(lambda (cells) (list (list (first cells) (second cells))))
                   cell-sequence)))

(defun filter-pairs (sequence-of-pairs pair-start-value)
  (remove pair-start-value sequence-of-pairs
          :test #'(lambda (test1 test2) (not (equal test1 (car test2))))))

(defun count-equal-pairs (sequence-of-pairs)
  (let* ((list-of-pairs (remove-duplicates sequence-of-pairs :test 'equal))
         (nr-of-pairs (mapcar #'(lambda (item-to-count) (count item-to-count sequence-of-pairs :test 'equal))
                              list-of-pairs))
         (weights (mapcar #'(lambda (factor) (/ factor (apply '+ nr-of-pairs))) nr-of-pairs)))
    (mapcar 'list list-of-pairs weights)
    ))

(defun scale-weigths-to-100% (weights)
  (let ((summan (apply '+ weights)))
    (mapcar #'(lambda (weight) (/ weight (if (= summan 0) 1 summan))) weights)))

(defun calculate-grid (tolerance length-of-sequence)
  (+ tolerance (/ 1 length-of-sequence)))

(defun check-if-weight-is-within-range (grid weight-from-table actual-weight)
  (let ((max-weight (+ grid weight-from-table))
        (min-weight (- weight-from-table grid)))
    (if (= weight-from-table 0)
      (= actual-weight 0)
      (and (>= max-weight actual-weight)
           (>= actual-weight min-weight)))))



(defun check-sequence-to-table (cell-sequence weights-table tolerance)
  (let* ((pairs (format-pairs-from-sequence cell-sequence))
         (current-pair-start (caar (last pairs)))
         (section-of-table (filter-weights-table weights-table current-pair-start)))
    (if section-of-table
      (let* ((pairs-for-analysis (filter-pairs pairs current-pair-start))
             (pairs-with-statistics (count-equal-pairs pairs-for-analysis))
             (actual-weights (mapcar #'(lambda (tablepair-with-statistic)
                                         (let ((answer (remove (car tablepair-with-statistic)
                                                               pairs-with-statistics
                                                               :test #'(lambda (test1 test2) (not (equal test1 (car test2)))))))
                                           (if answer (cadar answer) 0)))
                                     section-of-table))
             ;;actual weights are weights for pairs that exist in the table. Not found pairs from table will get 0 as weight.

             (scaled-actual-weights (scale-weigths-to-100% actual-weights))
             (weights-from-table (mapcar 'cadr section-of-table))
             (grid (calculate-grid tolerance (length pairs-for-analysis))))
        (eval (append '(and)
                      (mapcar #'(lambda (weight-from-table actual-weight)
                                  (check-if-weight-is-within-range grid weight-from-table actual-weight))
                              weights-from-table scaled-actual-weights))))
      t)
    ))


(defun markov-rule (layernr weights-table tolerance)

  (list
   #'(lambda (indexx x)
       (if (= layernr (get-layer-nr x))
         (let ((all-cells (append (get-all-variables-in-this-layer indexx x) (list (get-this-cell-from-svar x)))))
           (if (> (length all-cells) 1)
             (check-sequence-to-table all-cells weights-table tolerance)
             t))
         t))))


(om::defmethod! RC::r-markov ((layernr integer)
                              (cells list)
                              (weights list)
                              (tolerance number))
   :initvals '(1 nil  nil 0.0)
   :indoc '("layernr" "rythms" "weights" "max-deviation")
   :doc "This function was suggested by Paul Nauert.

The rule simulates a first-order Markov process. An element depends
on which element was selected to occupy the immediately preceding
position in the same layer. The selection is made according to
probabilities stored in a table.

The difference to a traditional Markov process can be explained with
an example: Two weights are given in the table, 50% and 50%.

In a traditional Markov process this means that the chanse for
either candidate is equal at any moment.

In this rule it means that it should be equal amount of both options
in the whole answer. The rule will check what the situation is up until
the timepoint he is looking at, and will choose the candidate that will
be accepted within the given tolerance.

<layernr> is the number for the rhythm layer the rule is valid for.
<cells> is a list of rhythmcells. All rhythmcells must exist in the
  layer's domain. The number of cells and their order correspond with
  the table in <weights>.
<weights> is a table of probabilities for the cells above. Each
  sub-list is a row in the table. The number of elemenst in each
  sub-list must equal the number of sub-lists (i.e. the number of
  columns must equal the number of rows. Each row is summed and
  normalized to 100 %.
<tolerance> is the maximum allowed deviation from the weights (example:
  0.1 is the same as 10 % deviation).

If there are cells that exist in the layer's domain but not in the <cells>
input they will always be accepted as a true answer, i.e. they will be
allowed at any point and they will not be affected by the rule. In this
way it is possible to temporary break the Markov-chain.

OBS: with a very small deviation you might not get a solution."
   :icon 352

   (markov-rule layernr
                (format-weights-table cells weights)
                tolerance))
