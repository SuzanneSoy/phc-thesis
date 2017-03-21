#lang scribble/manual @;or classicthesis
@require[@for-label[typed/racket/base]
          "util.rkt"]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)]{Thesis}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

@hyperlink["../pdf/phc-thesis.pdf"]{PDF version}

@include-section{state-of-the-art.scrbl}

@(generate-bibliography-section)