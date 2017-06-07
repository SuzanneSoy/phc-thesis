#lang at-exp racket
(provide typedracket Typedracket csharp CAML CLOS NIT CPP DeBruijn)

(require scribble/base
         scribble/core
         scribble/latex-properties
         scriblib/render-cond)

(define csharp-tex-addition
  (string->bytes/utf-8
   @string-append{
 \def\csharpAbbrev#1{
  \texorpdfstring{C\kern0.1ex\protect\raisebox{0.3ex}{\smaller $\#$}}{C\#}}}))

(define csharp
  (cond-element
   [html "C#"]
   [latex (elem #:style (style "csharpAbbrev"
                               (list (tex-addition csharp-tex-addition))) "")]
   [else "C#"]))

(define typedracket "Typed Racket")
(define Typedracket "Typed Racket")
(define CAML "CAML")
(define CLOS "CLOS")
(define NIT "NIT")
(define CPP "C++")
(define DeBruijn "De Bruijn")
