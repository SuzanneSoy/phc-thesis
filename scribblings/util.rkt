#lang racket

(provide (rename-out [my-title title])
         (rename-out [my-author+email author+email])
         asection
         atitle
         aquote
         quotation
         htodo
         todo
         (rename-out [~cite* ~cite])
         citet
         generate-bibliography-section
         (rename-out [note* note])
         define-footnote ;; TODO: does not use the (superscript …)
         (all-from-out "abbreviations.rkt")
         (all-from-out scribble-math)
         version-text
         aappendix
         tex-header
         tex-code
         include-section*
         include-asection
         struct-update
         part-style-update
         epigraph)

(require racket/stxparam
         racket/splicing
         scribble/base
         scribble/core
         scriblib/bibtex
         scriblib/footnote
         scriblib/render-cond
         racket/runtime-path
         scribble-enhanced/math
         scribble/latex-properties
         scribble/decode
         scribble-math
         phc-toolkit/untyped/meta-struct
         "abbreviations.rkt"
         (for-syntax syntax/parse)
         scribble/html-properties
         scribble/latex-properties)

(use-mathjax)

(define-syntax struct-update
  (syntax-parser
    [(_ struct-type:id v:expr [field:id updater:expr] ...)
     #'(let ([vv v])
         (struct-copy struct-type
                      vv
                      [field (updater ((struct-accessor struct-type field) vv))]
                      ...))]))
     

(define (tex-header . tex*)
  (elem #:style (style #f (list (tex-addition (string->bytes/utf-8
                                               (apply string-append tex*)))))))

(define scribble-tex-commands-addition
  (tex-addition
   (string->bytes/utf-8 "\\newcommand{\\scribbleTeXCommands}[1]{#1}")))

(define (tex-code code)
  (cond-element
   [latex (elem #:style (style "scribbleTeXCommands"
                               (list 'exact-chars
                                     scribble-tex-commands-addition))
                code)]
   [else (list)]))

(define (my-author+email author email)
  (cond-element
   [html (author+email author email)]
   ;; TODO: urlencode the email maybe?
   [latex (list (hyperlink (string-append "mailto:" email) author)
                (note (hyperlink (string-append "mailto:" email) email)))]
   [else (author+email author email)]
   ))

(define (version-text [prefix ""] [postfix ""])
  (with-handlers ([(λ (e) (eq? e 'git-failure))
                   (λ (e)
                     ;; unknown version (for now, just put the empty string).
                     "")])
    (define stderr (open-output-string))
    (define result
      (parameterize ([current-error-port stderr]
                     [current-input-port (open-input-string "")])
        (string-append
         prefix
         ;(tt
         (string-trim
          (with-output-to-string
           (λ ()
             (let ([git (find-executable-path "git")])
               (unless (system* git "show" "-s" "--date=short" "--format=%cd"
                                "HEAD")
                 (raise 'git-failure))))))
         "-" "-"
         (string-trim
          (with-output-to-string
           (λ ()
             (let ([git (find-executable-path "git")])
               (unless (system* git "rev-parse" "--short" "HEAD")
                 (raise 'git-failure))))))
         (if (non-empty-string?
              (string-trim
               (with-output-to-string
                (λ ()
                  (let ([git (find-executable-path "git")])
                    (unless (system* git "diff" "--shortstat")
                      (raise 'git-failure)))))))
             "x"
             "")
         "-" "-"
         (version);)
         postfix)))
    (if (non-empty-string? (get-output-string stderr))
        (begin (displayln (get-output-string stderr))
               (raise 'git-failure))
        result)))

;; TODO: merge the handling of unicode chars into scribble-math.
(define m
  (list setup-math
        (tex-header "\\renewcommand{\\rmdefault}{cmr}")))
(define my-title
  ;; TODO: use this for the other wrapped procs in this file
  (make-keyword-procedure
   (λ (kws kw-args . rest)
     (list m
           (keyword-apply title kws kw-args rest)))))

(define counter 0)
(define (counter!)
  (set! counter (add1 counter))
  counter)

(define (note* . content)
  (cond-element
   [html (let ([c (number->string (counter!))])
           (list (superscript c)
                 (apply note
                        (list (superscript c) ~ content))))]
   [latex (apply note content)]
   [else (apply note content)]))

(define-runtime-path bib-path "bibliography.bib")
(define-bibtex-cite bib-path
  ~cite
  citet
  generate-bibliography-section)

(define-syntax-parameter asection-current-level 0)
(define-syntax-rule (asection . body)
  (splicing-syntax-parameterize ([asection-current-level
                                  (add1 (syntax-parameter-value
                                         #'asection-current-level))])
    . body))

(define-syntax-parameter asection-current-is-appendix #f)
(define-syntax-rule (aappendix . body)
  (splicing-syntax-parameterize ([asection-current-is-appendix #t])
    (appendix)
    . body))


;; Alpha numbering of appendices in HTML and TeX
(define (num->alpha current parents)
  (define letters
    (vector->immutable-vector
     (vector-map
      symbol->string
      #(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))))

  (define cur (sub1 current))

  (define-values (oldlimit len)
    (let loop ([oldlimit 0]
               [limit 26]
               [len 1])
      (if (< cur limit)
          (values oldlimit len)
          (loop limit (+ (* limit 26) 26) (add1 len)))))
    
  (let-values ([(_ ans) (for/fold ([v (- cur oldlimit)]
                                   [ans '()])
                                  ([i (in-range len)])
                          (define-values (q r) (quotient/remainder v 26))
                          (values q
                                  (cons (vector-ref letters r) ans)))])
    (values (list (string-join ans "") ".")
            (add1 current))))
(define appendix-numberer
  (make-numberer num->alpha
                 1))

(define (part-style-update p updater)
  (define (style-updater old-style)
    (struct-update
     style
     old-style
     [properties updater]))
  (if (part-start? p)
      (struct-update part-start p [style style-updater])
      (struct-update part p [style style-updater])))

;; make-appendix-section must be called by @atitle below
(define (make-appendix-section p)
  (part-style-update p (λ (old-props)
                         (cons appendix-numberer
                               old-props))))
(define (appendix)
  (list
   (section #:style (style #f (list 'hidden 'toc-hidden 'unnumbered)))
   (tex-header
    "\\usepackage{alphalph}")
   (tex-code
    "\\appendix\\renewcommand\\thesection{\\AlphAlph{\\value{section}}}")))
 
(define-syntax (atitle stx)
  (syntax-case stx ()
    [(_ . args)
     (case (syntax-parameter-value #'asection-current-level)
       [(0) #'(my-title . args)]
       [(1) (if (syntax-parameter-value #'asection-current-is-appendix)
                #'(make-appendix-section (section . args))
                #'(section . args))]
       [(2) #'(subsection . args)]
       [(3) #'(subsubsection . args)]
       [else
        ;; TODO: log a warning here maybe?
        #'(subsubsub*section . args)])]))

(define-syntax (include-section* stx)
  (syntax-case stx ()
    [(_ mod)
     (with-syntax ([doc (datum->syntax #'mod 'doc #'mod)])
       #'(let ()
           (local-require (only-in mod doc))
           doc))]))

(define-syntax (include-asection stx)
  (syntax-case stx ()
    [(_ mod)
     (if (syntax-parameter-value #'asection-current-is-appendix)
         #'(make-appendix-section (include-section* mod))
         #'(include-section* mod))]))
  

;; hidden todo:
(define (htodo . args) (list))

;; todo:
(define (coloured-elem colour . content)
  (elem #:style (style #f (list (color-property colour))) content))
(define (todo . args)
  (list (coloured-elem "gray" "[")
        args
        (coloured-elem "gray" "]" (superscript "Todo"))))

(define (aquote . content)
  (nested-flow (style #f '())
               (list (paragraph (style #f '()) content))))

(define (quotation . content)
  (nested-flow (style #f '())
               (list (paragraph (style #f '()) content))))

(define (~cite* #:precision [precision #f] . rest)
  (if precision
      (list (apply ~cite rest) ", " precision)
      (apply ~cite rest)))

(define epigraph-css
  #"
.epigraphStyle p:last-child {
    padding-bottom: 0.2em;
    margin-bottom: 0;
}

.epigraphAuthorStyle p:first-child {
    padding-top: 0;
    margin-top: 0;
}

.epigraphOuter {
    text-align: right;
    display: table;
    margin-right: 0;
    padding-right: 0;
    margin-left: auto;
}

.epigraphStyle {
    display: table-cell;
    border-bottom: thin solid gray;
    font-style: italic;
}

.epigraphAuthorStyle {
    display: table-cell;
    padding-top: 0.5em;
}

.epigraphOuter > .SIntrapara {
    margin: 0;
    display: table-row; 
}

.epigraphOuter * {
    margin-right: 0;
    padding-right: 0;
    margin-left: 0;
    padding-left: 0;
}
")

(define epigraph-tex
  (string->bytes/utf-8
   #<<EOTEX
\usepackage{epigraph}
\usepackage{environ}
\def\lastepigraph{}
\def\lastepigraphauthor{}
\newenvironment{epigraphOuter}{}{}
\def\setepigraphwidth#1{\setlength{\epigraphwidth}{#1}}
\NewEnviron{epigraphStyle}{\global\let\lastepigraph\BODY}
\NewEnviron{epigraphAuthorStyle}{%
  \global\let\lastepigraphauthor\BODY%
  \epigraph{\emph{\lastepigraph}}{\lastepigraphauthor}%
}
EOTEX
   ))

(define epigraph-additions
  (list (css-addition epigraph-css)
        (tex-addition epigraph-tex)))

(define (epigraph #:width [width "6cm"] author . rest)
  (nested #:style (style "epigraphOuter"
                         '()
                         #;(list
                            (attributes
                             `([style . ,(format "max-width: ~a;" width)]))))
          (cond-element
           [latex (elem #:style (style "setepigraphwidth" '()) width)]
           [else (elem)])
          (apply nested #:style (style "epigraphStyle" epigraph-additions)
                 rest)
          (nested #:style (style "epigraphAuthorStyle" epigraph-additions)
                  author)))