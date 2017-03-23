#lang scribble/manual @;or classicthesis
@require[@for-label[typed/racket/base]
         "util.rkt"
         scriblib/render-cond
         racket/system
         racket/port
         racket/string
         scribble/core]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Thesis}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

@;@(version-text "Document version: " ".")

@(cond-element
  [html (list "Download a "
              @hyperlink["../pdf/phc-thesis.pdf"]{PDF version}
              " of this document.")]
  [latex
   (list "HTML version available at "
         @hyperlink["https://jsmaniac.github.io/phc-thesis/phc-thesis/"
                    ]{https://jsmaniac.github.io/phc-thesis/phc-thesis/}
                     ".")]
  [else ""])

@(table-of-contents)

@include-section*{state-of-the-art.scrbl}
@;{@(struct-update part
                (include-section* "state-of-the-art.scrbl")
                [style (λ (s)
                         (struct-update style s
                                        [properties (λ (p) (cons 'unnumbered p))]))])}
@;@include-asection{state-of-the-art.scrbl}

@;@(generate-bibliography-section)
@; Generate the bibliography with a numbered section:
@(part-style-update (generate-bibliography-section)
                    (λ (p) (remove 'unnumbered p)))

@(define default-nb
   (make-numberer (λ (a b)
                    (eprintf "(my-nb ~s ~s)\n" a b)
                    (values (string-join (append b (list (number->string a)))
                                         ".")
                            (add1 a)))
                  1))

@; experiments:
@;{
 @aappendix{
  @asection{
   @atitle{Fu}}
  @include-asection[
 (lib "phc-graph/scribblings/phc-graph-implementation.scrbl")]
  @asection{
   @atitle{Bar}
   @asection{
    @atitle[#:style (style #f (list default-nb))]{One}}
   @asection{
    @atitle[#:style (style #f (list default-nb))]{Two}}}

  @asection{
   @atitle{Hello world}
   Hi there!}
 }
}

@aappendix{
 @include-asection[(lib "phc-graph/scribblings/phc-graph-implementation.scrbl")]
 @include-asection[(lib "phc-adt/scribblings/phc-adt-implementation.scrbl")]
}