;;
;;
;;            Rhythm Constraints library version 1.41
;;
;;            Örjan Sandred,  © IRCAM 1999 (version 1.1)
;;            Örjan Sandred,  © Stockholm 2002 (version 1.3-)
;;
;;            update 1.41 January 2005
;;            update 1.3 august 2002
;;            this update adds the possibility to work with pauses in all functions/rules
;;            update 1.31 September 2002
;;            update 1.32 October 2002
;;            update 1.4 December 2004
;;


;--------------------------------------------------
(defvar RC)
(defpackage RC)
(defpackage omcs)
(in-package RC)


;--------------------------------------------------
;Variable definiton with files to load
;--------------------------------------------------

(defvar *OM_RC-lib-files* nil)
(setf *OM_RS-lib-files* '(
                          "CL:libraries;omrc;rc-sources;Classes-and-vectors.lisp"
                          "CL:libraries;omrc;rc-sources;simple-tree.lisp"
                          "CL:libraries;omrc;rc-sources;tree-simple.lisp"
                          "CL:libraries;omrc;rc-sources;Rules.lisp"
                          "CL:libraries;omrc;rc-sources;Build-domains.lisp"
                          "CL:libraries;omrc;rc-sources;decode.lisp"
                          "CL:libraries;omrc;rc-sources;measure-rules.lisp"
                          "CL:libraries;omrc;rc-sources;Access-lock-result.lisp"
                          "CL:libraries;omrc;rc-sources;Tools-user-rules.lisp"
                          "CL:libraries;omrc;rc-sources;Heuristic-rules.lisp"
                          "CL:libraries;omrc;rc-sources;RCUpdate.lisp"
                          "CL:libraries;omrc;rc-sources;markov-rule.lisp"
                          "CL:libraries;omrc;rc-sources;Kvantisering.lisp"
                          ))

(dolist (file *OM_RS-lib-files*)
  (assert (probe-file file)))

;--------------------------------------------------
;Loading files
;--------------------------------------------------

(mapc #'om::compile&load *OM_RS-lib-files*)


;--------------------------------------------------
; RC subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(defvar *subpackages-list* nil)
(setf *subpackages-list*
      '(
        ("01-build domains" nil nil (domains->pmc voice-domain preset-layer preset-timesign make-rhythm-domain) nil)
        ("02-decode solution" nil nil (decode-engine layer-in-solution view-presets) nil)
        ("03a-rules interface" nil nil (rules->pmc heuristicrules->pmc) nil)
        ("03b-rules" (("QUANTIZE" nil nil (r-quant hr-quant_dev hr-quant_ornaments) nil)) nil (r-hierarchy r-eqlength r-layerorder r-identical r-beat-subdiv r-sync-over-barline r-pattern r-canon r-markov
r-order-priority gr-hierarchy gr-eqlength gr-layerorder gr-identical gr-canon) nil)
        ("04-user rules tools" nil nil (get-this-cell get-this-cell-dur get-last-cell get-cell-before-last get-cell-two-before-last get-all-cells
get-cell-other-layer get-cell-any-layer get-rhythm-other-layer get-rhythm-any-layer get-all-cells-any-layer
get-time get-layernumber get-voicenumber pause? rhythmcell? timesign? test-equal test-not-equal) nil)
        ("05-lock sections" nil (stored-section) (store-section r-lock-to-stored decode-stored-section) nil)
        ("06-formating" nil nil (simpleformat->tree tree->simpleformat rhythmdomain->voices) nil)
        ))

;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library *subpackages-list*)
