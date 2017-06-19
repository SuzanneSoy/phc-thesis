#lang info
(define collection "phc-thesis")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '(("scribble-lib" #:version "1.20")
                     "racket-doc"
                     "typed-racket-lib"
                     "at-exp-lib"
                     "scribble-enhanced"
                     "scribble-math"
                     "phc-toolkit"
                     "srfi-doc"
                     "srfi-lite-lib"
                     "datalog"
                     "racklog"
                     "scribble-doc"
                     "slideshow-doc"
                     "r5rs-doc"
                     "r6rs-doc"
                     "redex-doc"
                     "turnstile"
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
