#lang racket

(require scribble/manual
         scribble/core
         scribble/latex-properties
         scribble/html-properties
         scribble-math
         scribble-math/katex-convert-unicode)

(provide mathtext
         clean-$
         (rename-out [$* $]
                     [$$* $$]))

(define (mathtext . l)
  ;; TODO: (clean-$ l), but wrap converted stuff with $…$
  (apply elem #:style (style "mathText"
                             (list (tex-addition #"\\def\\mathText#1{#1}")
                                   'exact-chars))
         l))

;; Math stuff
(define (clean-$ e)
  (cond [(pair? e) (cons (clean-$ (car e)) (clean-$ (cdr e)))]
        [(traverse-element? e)
         (traverse-element (λ (a b)
                             (clean-$ ((traverse-element-traverse e) a b))))]
        [(match e
           [(element (style (or "math" "texMathInline" "texMathDisplay") _)
                     content)
            #t]
           [_ #f])
         ;; No need to go down recursively, as the contents should already have
         ;; been cleaned when the e was created. Plus we risk re-escaping
         ;; things within \text{…}.
         (element-content e)]
        [(match e [(element (style "mathWrapper" _) _) #t]
           [_ #f])
         (clean-$ (element-content e))]
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
                  (clean-$ (element-content e)))]
        [(string? e)
         ;; TODO: do this only when compiling to HTML.
         (katex-convert-unicode e)]
        [else e]))

(define (math-wrapper $m)
  (element (style "mathWrapper"
                  (list (tex-addition #"\\def\\mathWrapper#1{#1}")
                        (css-addition #".mathWrapper { font-size: medium; }")))
           $m))

(define ($* . elts)
  (math-wrapper (apply $ (clean-$ elts))))

(define ($$* . elts)
  (math-wrapper (apply $$ (clean-$ elts))))
