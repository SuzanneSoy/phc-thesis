#lang info
(define collection "phc-thesis")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "typed-racket-lib"
                     "at-exp-lib"
                     "scribble-enhanced"
                     "scribble-math"))
(define scribblings '(("scribblings/phc-thesis.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(georges))
