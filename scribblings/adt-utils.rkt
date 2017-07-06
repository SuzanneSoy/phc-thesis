#lang at-exp racket
@(provide (all-defined-out))
@require["util.rkt"
         (for-label (only-meta-in 0 typed/racket))]


@(require racket/list
          (for-syntax racket/base syntax/parse racket/list))
@(begin-for-syntax
   (define-syntax-class stringify
     [pattern x:id #:when (identifier-binding #'x)
      #:with →str #'x]
     [pattern x:id #:with →str (datum->syntax #'x
                                              (symbol->string (syntax-e #'x))
                                              #'x)]
     [pattern e #:with →str #'e]))
@(define-for-syntax (defop op)
   (syntax-parser
     [(_ :stringify ...)
      #`@${(@(add-between (list (string-append "\\textbf{" #,op "}")
                                #,@(syntax->list #'(→str ...)))
                          "\\ "))}]))
@(define-syntax stringify
   (syntax-parser
     [(_ :stringify)
      #'→str]))

@(define-syntax ctor (defop "ctor"))
@(define κ @${κ})
@(define ɐ @${a})
@(define-syntax ctorTop (defop "CtorTop"))
@(define-syntax-rule (ctor-val κ)
   @${\mathbf{getval}_@κ})
@(define-syntax record (defop "record"))
@(define-syntax variant (defop "V"))
@(define (repeated . l) @${\overrightarrow{@l}})
@(define (repeatset . l) @${\{\overrightarrow{@l}\}})
@(define |P| @${\ |\ })
@(define ρc @${\rho_{c}})
@(define ρf @${\rho_{f}})
@(define-syntax atc (defop "@${}_{\\textbf{c}}$"))
@(define-syntax atf (defop "@${}_{\\textbf{f}}$"))
@(define-syntax-rule (ctor-pred c)
   @${@(stringify c)\mathbf{?}})
@(define-syntax-rule (record-pred f*)
   @${(\textbf{record?}\ @(stringify f*))})
@(define-syntax-rule (opwith rec a v)
   @list{@(stringify rec) \textbf{ with } @(stringify a) = @(stringify v)})
@(define-syntax-rule (opwithout rec a)
   @list{@(stringify rec) \textbf{ without } @(stringify a)})
@(define πctor-val @${\textbf{getval}})
@(define (πɐ . ɐ) @${\mathbf{@ɐ}})