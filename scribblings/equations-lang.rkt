#lang racket/base

(require racket/require
         (for-syntax (rename-in racket/base [compose ∘])
                     syntax/stx
                     syntax/parse)
         (subtract-in scribble/manual "../scribblings/util.rkt")
         "../scribblings/util.rkt"
         "../scribblings/abbreviations.rkt"
         "../scribblings/adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket)
                    typed/racket/class)
         (only-in scribble/base emph)
         scribble/example
         racket/string)

(define tr-eval (make-eval-factory '(typed/racket)))

(provide (rename-out [-#%module-begin #%module-begin])
         tr-eval
         (except-out (all-from-out racket/base) #%module-begin)
         (all-from-out scribble/manual
                       "../scribblings/util.rkt"
                       "../scribblings/abbreviations.rkt"
                       "../scribblings/adt-utils.rkt"
                       scribble/base
                       scribble/example
                       racket/string)
         (for-label (all-from-out typed/racket)
                    (all-from-out typed/racket/class)))

(define-syntax -#%module-begin
  (syntax-parser
    [(_ {~optional {~seq {~and def {~not :keyword}} ... #:}}
        {~and body {~not :keyword}} ...
        {~seq namekw:keyword {~and nbody {~not :keyword}} ...}
        ...)
     #:with (name ...) (stx-map (∘ string->symbol keyword->string syntax-e)
                                #'(namekw ...))
     #:with (ntmp ...) (generate-temporaries #'(name ...))
     #`(#%module-begin
        (provide (rename-out [ntmp name] ...))
        #,@(if (attribute def) #'{def ...} #'{})
        (define tmp (list body ...))
        (define ntmp (list nbody ...))
        ...
        (module+ equations
          (provide (rename-out [tmp equations]))))]))
