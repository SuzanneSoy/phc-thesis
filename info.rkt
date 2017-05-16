#lang info
(define collection "phc-thesis")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "typed-racket-lib"
                     "at-exp-lib"
                     "scribble-enhanced"
                     "scribble-math"
                     "phc-toolkit"
                     ;; Literate programming dependencies:
                     ;; phc-graph:
                     "aful"
                     "delay-pure"
                     "phc-adt-doc"
                     "https://github.com/jsmaniac/phc-graph.git" ;; "phc-graph"
                     "stxparse-info"
                     "subtemplate"
                     "type-expander"
                     "typed-racket-doc"
                     "typed-worklist"
                     ;; phc-adt:
                     "alexis-util"
                     "extensible-parser-specifications"
                     "multi-id"
                     "remember"
                     "threading-doc"
                     "trivial"
                     "typed-struct-props"
                     "xlist"
                     ;; remember, multi-id, type-expander:
                     "auto-syntax-e"
                     "mutable-match-lambda"
                     ))
(define scribblings '(("scribblings/phc-thesis.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(georges))
