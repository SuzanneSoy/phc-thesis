#lang scribble/manual

@require["util.rkt"
         (for-label racket)]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Extensible type system and algebraic datatypes}

@include-asection{te.scrbl}
@include-asection{adt.scrbl}