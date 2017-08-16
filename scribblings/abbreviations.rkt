#lang at-exp racket
(provide typedracket Typedracket csharp CAML OCAML CLOS NIT CPP DeBruijn HOAS
         PHOAS monocecil dotnet DLL nanopass nanopass-c-f haskell haskell98
         Hackett turnstile Turnstile cur Cur LaTeX C-language java lisp)

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
(define nanopass "Nanopass")
(define nanopass-c-f "Nanopass Compiler Framework")
(define CAML "CAML")
(define OCAML "CAML")
(define CLOS "CLOS")
(define NIT "NIT")
(define CPP "C++")
(define DeBruijn "De Bruijn")
;; TODO: make it an <abbr> and give it a tooltip in HTML
(define HOAS "HOAS")
(define PHOAS "PHOAS")
(define monocecil @tt{"Mono.Cecil"})
(define dotnet ".NET")
(define DLL "DLL")
(define haskell "Haskell")
(define haskell98 "Haskell 98")
(define Hackett "Hackett")
(define turnstile "Turnstile")
(define Turnstile "Turnstile")
(define cur "Cur")
(define Cur "Cur")
(define LaTeX (cond-element [latex (elem #:style (style "LaTeX" '()))]
                            [else "LaTeX"]))
(define C-language "C")
(define java "Java")
(define lisp "Lisp")