#lang scribble/manual @;or classicthesis
@require[@for-label[typed/racket/base]
         "util.rkt"
         scriblib/render-cond
         racket/system
         racket/port
         racket/string
         scribble/core]
@(use-mathjax)

@;@tex-header{\usepackage{morewrites}}
@tex-header{
  \let\realtableofcontents\tableofcontents
  \def\tableofcontents{\realtableofcontents\let\tableofcontents\relax}
}

@title[#:style (struct-update style
                              (with-html5 manual-doc-style)
                              [properties (λ (p) (cons 'toc p))])
       #:version (version-text)]{Thesis}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

@;@(version-text "Document version: " ".")

@(cond-element
  [html @list{Download a @hyperlink["../pdf/phc-thesis.pdf"]{PDF version}
    of this document.

    Download a @hyperlink[(version-text "../phc-thesis-" ".zip")]{Zip archive}
    of the HTML and PDF versions of this document.}]
  [latex
   (list "HTML version available at "
         @hyperlink["https://jsmaniac.github.io/phc-thesis/phc-thesis/"
                    ]{https://jsmaniac.github.io/phc-thesis/phc-thesis/}
                     ".")]
  [else @list{HTML version: https://jsmaniac.github.io/phc-thesis/phc-thesis/

    PDF version: https://jsmaniac.github.io/phc-thesis/pdf/phc-thesis.pdf

    Zip archive of the HTML and PDF versions:
    @(version-text "https://jsmaniac.github.io/phc-thesis/phc-thesis-"
                   ".zip")}])

@(table-of-contents)

@include-asection{introduction.scrbl}
@include-asection{state-of-the-art.scrbl}
@include-asection{initial-examples.scrbl}
@include-asection{tr.scrbl}
@include-asection{tr-te-adt.scrbl}
@asection{ @atitle{Typed nanopass}
 @asection{@atitle[#:tag "typed-nanotrees-chap"]{Typed nanopass on trees}}
 @asection{@atitle[#:tag "typed-nanodags-chap"]{Typed nanopass on DAGs}}
 @asection{@atitle[#:tag "typed-nanographs-chap"]{Typed nanopass on graphs}}
 @asection{@atitle[#:tag "structural-invariants-chap"]{Structural invariants}}
 @asection{@atitle[#:tag "future-extensions-chap"]{Further possible
   extensions}} }
@asection{@atitle[#:tag "results-chap"]{Examples and results}}
@asection{@atitle[#:tag "conclusion-future-chap"]{Conclusion and future work
  directions}}

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

@(require (for-syntax racket/base
                      mzlib/etc))
@(define-syntax (if-appendices stx)
   (syntax-case stx ()
     [(_ a)
      (if (file-exists? (build-path (this-expression-source-directory)
                                    'up
                                    "no-appendices"))
          #'(displayln "Appendices turned off on this build."
                       (current-error-port))
          #`(begin
              (displayln "Appendices enabled on this build."
                         (current-error-port))
              a))]))
@if-appendices[
 @aappendix{
  @include-asection[
 (lib "phc-graph/scribblings/phc-graph-implementation.scrbl")]
  @include-asection[(lib "phc-adt/scribblings/phc-adt-implementation.scrbl")]
  @include-asection[
 (submod (lib "remember/remember-implementation.hl.rkt") doc)]
  @include-asection[(submod (lib "multi-id/multi-id.hl.rkt") doc)]
  @include-asection[
 (lib "type-expander/scribblings/type-expander-implementation.scrbl")]
 }]

@;{
 Notes concerning tikz → SVG conversion:

 To get smooth (non-selectable) text with the LaTeX fonts:

 \documentclass[tikz]{standalone}
 %\usetikzlibrary{…}
 \begin{document}
 \begin{tikzpicture}
 \draw (0,0) -- (4,4);
 \node at (2,2) {Some Text};
 \end{tikzpicture}
 \end{document}

 then:
 pdflatex file.tex
 pdf2svg file.pdf file.svg

 See also:
 http://tex.stackexchange.com/questions/51757/
   how-can-i-use-tikz-to-make-standalone-svg-graphics


 To get real, selectable text (must not contain formatting, otherwise some
 <span> get inserted inside the SVG <text>, and that is incorrect), but without
 the LaTeX fonts. If we insert
 \tikzset{every node/.style={/pgf/tex4ht node/escape=true}}, then it is possible
 to have slightly fancier things in the text (e.g. colours), but it still is
 half-broken (a simple $alpha$ prints out a spurious display=”inline”).


 Insert right after the documentclass:
 \def\pgfsysdriver{pgfsys-tex4ht.def}
 
 htlatex error:
 http://tex.stackexchange.com/a/232739

 If the text is set to an opacity of 0, then it seems to still be selectable.
 It should therefore be possible to overlay the two SVGs, in order to get
 selectable text, with the specific LaTeX fonts.

 We might want to create a special scribble command to typeset "normal" text
 (rendered using scribble's HTML font), and another to typeset math (possibly
 with MathJax).
}
