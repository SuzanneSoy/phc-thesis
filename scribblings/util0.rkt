#lang racket

(require scribble/manual
         scribble/core
         scribble/latex-properties
         scribble/html-properties
         scribble-math
         scribble-math/mathjax-convert-unicode
         scriblib/render-cond)

(provide mathtext
         $p
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
           [(element (style (or "math"
                                (regexp #px"^texMathInline")
                                (regexp #px"^texMathDisplay"))
                            _)
                     content)
            #t]
           [_ #f])
         ;; No need to go down recursively, as the contents should already have
         ;; been cleaned when the e was created. Plus we risk re-escaping
         ;; things within \text{…}.
         (if mathmode?
             (element-content e)
             (list "$" (element-content e) "$"))]
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
         (mathjax-convert-unicode e mathmode?)]
        [else e]))

(define ($* . elts)
  (apply $ (clean-$ elts #t)))

(define ($$* . elts)
  (apply $$ (clean-$ elts #t)))

(define tex-mathpar
  (string->bytes/utf-8 #<<EOTEX
\def\texMathDisplayMathpar#1{\ifmmode #1\else\begin{mathpar}#1\end{mathpar}\fi}
EOTEX
                       ))

(define math-mathpar-style-latex
  (style "texMathDisplayMathpar" ;; MUST start with texMathDisplay.
         (list (tex-addition tex-mathpar)
               'exact-chars)))

(define $p-css
  (string->bytes/utf-8 #<<EOF
.mathpar {
    text-align: center;
    margin-top: 0.25em;
    margin-bottom: 0.25em;
    margin-left: -1.5em;
    margin-right: -1.5em;
}

.mathpar .MathJax_Display {
    width: auto;
    display: inline-block!important;
    margin-top: 0.75em;
    margin-bottom: 0.75em;
    margin-left: 1.5em;
    margin-right: 1.5em;
}

.mathpar::after {
    clear: both;
    content: "";
    display: block;
}
EOF
                 ))
(define $p-html-style
  (style "mathpar" (list (alt-tag "div") (css-addition $p-css))))

(define ($p . elts)
  (cond-element
   [latex (apply $$ #:latex-style math-mathpar-style-latex
                 (clean-$ (add-between elts "\\and") #t))]
   [else (apply elem #:style $p-html-style elts)]))
