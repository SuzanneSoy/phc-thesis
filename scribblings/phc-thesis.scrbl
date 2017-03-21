#lang scribble/manual @;or classicthesis
@require[@for-label[typed/racket/base]
         "util.rkt"
         scriblib/render-cond
         racket/system
         racket/port
         racket/string]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Thesis}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

@;@(version-text "Document version: " ".")

@(cond-element
  [html (list "Download a "
              @hyperlink["../pdf/phc-thesis.pdf"]{PDF version}
              ".")]
  [latex
   (list "HTML version available at "
         @hyperlink["https://jsmaniac.github.io/phc-thesis/phc-thesis/"
                    ]{https://jsmaniac.github.io/phc-thesis/phc-thesis/}
         ".")]
  [else ""])

@include-section{state-of-the-art.scrbl}

@(generate-bibliography-section)