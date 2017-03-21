#lang racket

(provide (rename-out [my-title title])
         asection
         atitle
         aquote
         quotation
         htodo
         todo
         (rename-out [~cite* ~cite])
         citet
         generate-bibliography-section
         (rename-out [note* note])
         define-footnote ;; TODO: does not use the (superscript …)
         (all-from-out "abbreviations.rkt")
         (all-from-out scribble-math))

(require racket/stxparam
         racket/splicing
         scribble/base
         scribble/core
         scriblib/bibtex
         scriblib/footnote
         scriblib/render-cond
         racket/runtime-path
         scribble-enhanced/math
         scribble/latex-properties
         scribble-math
         "abbreviations.rkt")

(use-mathjax)

(define (tex-header tex)
  (elem #:style (style #f (list (tex-addition tex)))))

;; TODO: merge the handling of unicode chars into scribble-math.
(define m
  (list setup-math
        (tex-header "\renewcommand{\rmdefault}{cmr}")))
(define my-title
  ;; TODO: use this for the other wrapped procs in this file
  (make-keyword-procedure
   (λ (kws kw-args . rest)
     (list m
           (keyword-apply title kws kw-args rest)))))

(define counter 0)
(define (counter!)
  (set! counter (add1 counter))
  counter)

(define (note* . content)
  (cond-element
   [html (let ([c (number->string (counter!))])
           (list (superscript c)
                 (apply note
                        (list (superscript c) ~ content))))]
   [latex (apply note content)]
   [else (apply note content)]))

(define-runtime-path bib-path "bibliography.bib")
(define-bibtex-cite bib-path
  ~cite
  citet
  generate-bibliography-section)

(define-syntax-parameter asection-current-level 0)
(define-syntax-rule (asection . body)
  (splicing-syntax-parameterize ([asection-current-level
                                  (add1 (syntax-parameter-value
                                         #'asection-current-level))])
    . body))

(define-syntax (atitle stx)
  (syntax-case stx ()
    [(_ . args)
     (case (syntax-parameter-value #'asection-current-level)
       [(0) #'(my-title . args)]
       [(1) #'(section . args)]
       [(2) #'(subsection . args)]
       [(3) #'(subsubsection . args)]
       [else
        ;; TODO: log a warning here maybe?
        #'(subsubsub*section . args)])]))

;; hidden todo:
(define (htodo . args) (list))

;; todo:
(define (coloured-elem colour . content)
  (elem #:style (style #f (list (color-property colour))) content))
(define (todo . args)
  (list (coloured-elem "gray" "[")
        args
        (coloured-elem "gray" "]" (superscript "Todo"))))

(define (aquote . content)
  (nested-flow (style #f '())
               (list (paragraph (style #f '()) content))))

(define (quotation . content)
  (nested-flow (style #f '())
               (list (paragraph (style #f '()) content))))

(define (~cite* #:precision [precision #f] . rest)
  (if precision
      (list (apply ~cite rest) ", " precision)
      (apply ~cite rest)))