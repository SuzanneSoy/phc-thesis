#lang scribble/manual @;or classicthesis
@require[@for-label[typed/racket/base]
         "util.rkt"
         scriblib/render-cond
         racket/system
         racket/port
         racket/string]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)]{Thesis}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

@(with-handlers ([(λ (e) (eq? e 'git-failure))
                  (λ (e)
                    ;; unknown version (for now, just put the empty string).
                    "")])
   (define stderr (open-output-string))
   (define result
     (parameterize ([current-error-port stderr]
                    [current-input-port (open-input-string "")])
       (list
        "Document version: "
        (tt
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
             ""))
        ".")))
   (if (non-empty-string? (get-output-string stderr))
       (begin (displayln (get-output-string stderr))
              (raise 'git-failure))
       result))


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