#lang racket

(require scribble/manual
         scribble/core
         scribble/latex-properties
         scribble/html-properties
         scribble-math
         scribble-math/katex-convert-unicode)

(provide mathtext
         (rename-out [$* $]
                     [$$* $$]))

(define (mathtext . l)
  ;; TODO: (clean-$ l), but wrap converted stuff with $…$
  (apply elem #:style (style "mathText"
                             (list (tex-addition #"\\def\\mathText#1{#1}")
                                   'exact-chars))
         (clean-$ l #f)))

;; Math stuff
(define (clean-$ e mathmode?)
  (cond [(pair? e) (cons (clean-$ (car e) mathmode?)
                         (clean-$ (cdr e) mathmode?))]
        [(traverse-element? e)
         (traverse-element (λ (a b)
                             (clean-$ ((traverse-element-traverse e) a b)
                                      mathmode?)))]
        [(match e
           [(element (style (or "math" "texMathInline" "texMathDisplay") _)
                     content)
            #t]
           [_ #f])
         ;; No need to go down recursively, as the contents should already have
         ;; been cleaned when the e was created. Plus we risk re-escaping
         ;; things within \text{…}.
         (element-content e)]
        [(match e
           [(element (style "mathText" _)
                     content)
            #t]
           [_ #f])
         ;; No need to go down recursively, as the contents should already have
         ;; been cleaned when the e was created. Plus we risk re-escaping
         ;; things within \text{…}.
         ;; TODO: when a "math" "texMathInline" "texMathDisplay" is encountered
         ;; within a "mathText", we should wrap it with $…$.
         (element-content e)]
        [(element? e)
         (element (element-style e)
                  (clean-$ (element-content e)
                           mathmode?))]
        [(string? e)
         ;; TODO: do this only when compiling to HTML.
         (katex-convert-unicode e mathmode?)]
        [else e]))

(define ($* . elts)
  (apply $ (clean-$ elts #t)))

(define ($$* . elts)
  (apply $$ (clean-$ elts #t)))
