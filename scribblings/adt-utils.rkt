#lang at-exp racket
@(provide (except-out (all-defined-out)
                      num-e*
                      num-v*
                      num-τ*))
@require["util.rkt"
         scriblib/render-cond
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
@(define-for-syntax (defop* op)
   (syntax-parser
     [(_ :stringify ...)
      #`@${(@(add-between (list @textbf{@#,op}
                                #,@(syntax->list #'(→str ...)))
                          "\\ "))}]
     [self
      (identifier? #'self)
      #`@textbf{@#,op}]))

@(begin-for-syntax
   (require (for-syntax racket/base))
   (define-syntax (defop stx)
     (syntax-case stx ()
       [(_ op)
        (with-syntax ([s (datum->syntax #'op 'syntax)])
          #'(defop* (s op)))])))

@(define-syntax stringify
   (syntax-parser
     [(_ :stringify)
      #'→str]
     [(_ :stringify ...)
      #'(add-between (list →str ...) "\\ ")]))

(define (spaces . l)
  (add-between l "\\ "))

(define-syntax ctor (defop "ctor"))
(define κ @${κ})
(define ɐ @${a})
(define-syntax ctorTop (defop "CtorTop"))
(define-syntax-rule (ctor-val κ)
  @${\mathbf{getval}_@κ})
(define-syntax record (defop "record"))
(define-syntax variant (defop "V"))
(define-syntax ifop (defop "if"))
(define-syntax-rule (λv env (arg ...) expr)
  @${[@(stringify env), λ(@(list (stringify arg) ...)).@(stringify expr)]})
(define-syntax-rule (λe (arg ...) expr)
  @${λ(@(spaces (stringify arg) ...)).@(stringify expr)})
(define-syntax-rule (Λe (arg ...) expr)
  @${Λ(@(spaces (stringify arg) ...)).@(stringify expr)})
(define-syntax-rule (Λv env (arg ...) expr)
  @${[@(stringify env), Λ(@(spaces (stringify arg) ...)).@(stringify expr)]}) ;; TODO: is the env necessary here? It's a type env, right?
(define (repeated . l) @${\overrightarrow{@l}})
(define (repeatset . l)
  (cond-element
   [html
    @${\def\overrightbracedarrow#1{\overset{\scriptscriptstyle{\raise1mu{\{}}}{\vphantom{#1}}\overrightarrow{#1}\overset{\scriptscriptstyle{\raise1mu{\}}}}{\vphantom{#1}}}\overrightbracedarrow{@l}}]
   [else
    ;; Defined in util.rkt
    @${\overrightbracedarrow{@l}}]))
(define |P| @${\ |\ })
(define ρc @${\rho_{c}})
(define ρf @${\rho_{f}})
(define-syntax at (defop "@"))
(define-syntax atc (defop (list "@" @${{}_{@textbf{c}}})))
(define-syntax atf (defop (list "@" @${{}_{@textbf{f}}})))
(define-syntax Λc (defop (list "Λ" @${{}_{@textbf{c}}})))
(define-syntax Λf (defop (list "Λ" @${{}_{@textbf{f}}})))
(define-syntax ∀r (defop @${\mathbf{∀}}))
(define-syntax ∀c (defop @${\mathbf{∀}_{@textbf{c}}}))
(define-syntax ∀f (defop @${\mathbf{∀}_{@textbf{f}}}))
(define-syntax-rule (ctor-pred c)
  @${@(stringify c)\mathbf{?}})
(define-syntax-rule (record-pred . f*)
  @${(@textbf{record?}\ @(stringify . f*))})
(define-syntax-rule (record-pred* . f*)
  @${(@textbf{record$\mathbf{*}$?}\ @(stringify . f*))});; TODO text vs math!!!
(define-syntax-rule (opwith rec a v)
  @list{@(stringify rec) @textbf[" with "] @(stringify a) = @(stringify v)})
(define-syntax-rule (opwithout rec a)
  @list{@(stringify rec) @textbf[" without "] @(stringify a)})
(define πctor-val @${@textbf{getval}})
(define (πɐ . ɐ) @${\mathbf{@ɐ}})
;; Associates a variable name or record field name with a value, type or
;; evaluation context.
(define-syntax-rule (↦v name val)
  @${@(stringify name)↦@(stringify val)})
(define-syntax-rule (↦e name val) @${@(stringify name)↦@(stringify val)})
(define-syntax-rule (↦E name val) @${@(stringify name)↦@(stringify val)})
(define-syntax-rule (↦τ name val) @${@(stringify name)↦@(stringify val)})
(define-syntax-rule (app f+args ...)
  @${(@(add-between (list (stringify f+args) ...) "\\ "))})

(define-syntax num-e* (defop "num"))
(define-syntax num-v* (defop "num"))
(define-syntax num-τ* (defop "num"))
(define num-e @num-e*[n])
(define num-v @num-v*[n])
(define num-τ @num-τ*[n])

(define-syntax true-e (defop "true"))
(define-syntax true-v (defop @${@textit{true}}))
(define-syntax true-τ (defop "true"))

(define-syntax false-e (defop "false"))
(define-syntax false-v (defop @${@textit{false}}))
(define-syntax false-τ (defop "false"))

(define-syntax un (defop "∪"))

(define-syntax-rule (f→ (from ...) R)
  @${(@(add-between (list @(stringify from) ...) "\\ ") → @(stringify R))})
(define-syntax (R stx)
  (syntax-case stx ()
    [(_ to φ⁺ φ⁻ o)
     #'@${❲@(stringify to)
      ; @(stringify φ⁺) / @(stringify φ⁻)
      ; @(stringify o)❳}]
    [self (identifier? #'self)
     #'@${\mathrm{R}}]))
#;(define-syntax-rule (f→ (from ...) to φ O)
    @${
 (@list[@(stringify from) ...]
 \xrightarrow[@(stringify O)]{@(stringify φ)}
 @(stringify to))
 })

(define primop "p")

(define-syntax conse (defop "cons"))
(define-syntax-rule (consv a b) @${⟨@(stringify a), @(stringify b)⟩})
(define-syntax-rule (consτ a b) @${⟨@(stringify a), @(stringify b)⟩})
(define-syntax-rule (polydot τ α)
  @${@(stringify τ) \mathbf{…}_{@(stringify α)}})
(define-syntax-rule (polydotα α)
  @${@(stringify α) \mathbf{…}})
@;(define-syntax →Values (defop "Values"))
(define-syntax-rule (→Values v ...) (spaces (stringify v) ...))
(define @emptypath @${ϵ} #;@${•})
(define-syntax-rule (<: a b)
  @${⊢ @(stringify a) \mathrel{<:} @(stringify b)})

(define-syntax-rule (<:R a b)
  @${⊢ @(stringify a) \mathrel{{<:}_R} @(stringify b)})

@(define-syntax (Γ stx)
   (syntax-case stx (⊢)
     [(_ more ... ⊢ x τ φ+ φ- o)
      #'@${@(add-between (list "Γ" (stringify more) ...) ", ") ⊢
       @(stringify x)
       : @(stringify τ)
       ; @(stringify φ+) / @(stringify φ-)
       ; @(stringify o)}]))