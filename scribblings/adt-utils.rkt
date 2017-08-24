#lang at-exp racket
(provide (except-out (all-defined-out)
                     num-e*
                     num-τ*
                     List…τ*
                     Λc*
                     Λf*))

@require["util.rkt"
         (only-in scribble/base emph)
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
          #'(defop* (s op)))]))

   (define-syntax (defvop stx)
     (syntax-case stx ()
       [(_ op)
        (with-syntax ([s (datum->syntax #'op
                                        `@${\mathord{@textbfit{@,#'op}}})])
          #'(defop s))])))

@(define-syntax stringify
   (syntax-parser
     [(_ :stringify)
      #'→str]
     [(_ :stringify ...)
      #'(add-between (list →str ...) "\\ ")]))

(define (spaces . l)
  (add-between l "\\ "))

(define-syntax ctorτ (defop "Ctor"))
(define-syntax ctorv (defvop "ctor"))
(define-syntax ctore (defop "ctor"))
(define κ @${κ})
(define-syntax κof
  (syntax-rules ()
    [(_ τ) @${@|κ|\ \mathbf{of}\ @(stringify τ)}]
    [(_ i τ) @${@|κ|@(stringify i)\ \mathbf{of}\ @(stringify τ)}]))
(define-syntax κe
  (syntax-rules ()
    [(_ e) @${@|κ|\ @(stringify e)}]
    [(_ i e) @${@|κ|@(stringify i)\ @(stringify e)}]))
(define-syntax κv
  (syntax-rules ()
    [(_ v) @${@|κ|\ @(stringify v)}]
    [(_ i v) @${@|κ|@(stringify i)\ @(stringify v)}]))
(define ɐ @${a})
(define-syntax ctorTop (defop "CtorTop"))
(define-syntax-rule (ctor-val κ)
  @${\mathbf{getval}_@κ})
(define-syntax recordτ (defop "Record"))
(define-syntax recordv (defvop "record"))
(define-syntax recorde (defop "record"))
(define-syntax variantτ (defop "V"))
(define-syntax ifop (defop "if"))
(define-syntax mapop (defop "map"))
(define-syntax-rule (λv (arg ...) expr)
  @${@mathbfit{λ}(@(list (stringify arg) ...)).@(stringify expr)})
(define-syntax-rule (λe (arg ...) expr)
  @${\mathbf{λ}(@(spaces (stringify arg) ...)).@(stringify expr)})
(define-syntax-rule (Λe (arg ...) expr)
  @${\mathbf{Λ}(@(spaces (stringify arg) ...)).@(stringify expr)})
(define-syntax-rule (Λv (arg ...) expr)
  @${@mathbfit{Λ}(@(spaces (stringify arg) ...)).@(stringify expr)}) ;; TODO: is the env necessary here? It's a type env, right?
(define (repeated #:n [n #f] . l)
  (if n
      @${\overrightarrow{@l}\overset{\scriptscriptstyle\,\ifmathjax{\raise1mu{@|n|}}\iflatex{@|n|}}{\vphantom{@l}}}
      @${\overrightarrow{@l}}))
(define (repeatset #:w [wide? #f] . l)
  (define w (if wide? "\\!" ""))
  ($ (cond-element
      [html
       @${\def\overrightbracedarrow#1{\overset{\scriptscriptstyle{\raise1mu{\{}}}{\vphantom{#1}}\overrightarrow{@|w|#1@|w|}\overset{\scriptscriptstyle{\raise1mu{\}}}}{\vphantom{#1}}}\overrightbracedarrow{@l}}]
      [else
       ;; Defined in util.rkt
       @${\overrightbracedarrow{@|w|@|l|@|w|}}])))
(define (repeatSet . l)
  @${\{@(apply repeatset l)\}})
(define |P| @${\ |\ })
(define ρc @${ρ_{c}})
(define ρf @${ϱ_{f}})
(define ςc @${ς_{c}})
(define ςf @${ς_{f}})
(define-syntax at (defop "@"))
(define-syntax atc (defop (list "@" @${{}_{@textbf{c}}})))
(define-syntax atf (defop (list "@" @${{}_{@textbf{f}}})))
(define-syntax Λc* (defop (list "Λ" @${{}_{@textbf{c}}})))
(define-syntax Λf* (defop (list "Λ" @${{}_{@textbf{f}}})))
(define-syntax-rule (Λce (ρ ...) e)
  (Λc* @${(@(add-between (list ρ ...) "\\ "))} e))
(define-syntax-rule (Λcv (ρ ...) e)
  (Λc* @${(@(add-between (list ρ ...) "\\ "))} e))
(define-syntax-rule (Λfe (ρ ...) e)
  (Λf* @${(@(add-between (list ρ ...) "\\ "))} e))
(define-syntax-rule (Λfv (ρ ...) e)
  (Λf* @${(@(add-between (list ρ ...) "\\ "))} e))
(define-syntax ∀r* (defop @${@mathbm{∀}}))
(define-syntax-rule (∀r (α ...) τ)
  (∀r* @${(@(add-between (list (stringify α) ...) "\\ "))} τ))
(define-syntax ∀c* (defop @${@mathbm{∀}_{@textbf{c}}}))
(define-syntax ∀f* (defop @${@mathbm{∀}_{@textbf{f}}}))
(define-syntax-rule (∀c (ρ ...) τ)
  (∀c* @${(@(add-between (list (stringify ρ) ...) "\\ "))} τ))
(define-syntax-rule (∀f (ρ ...) τ)
  (∀f* @${(@(add-between (list (stringify ρ) ...) "\\ "))} τ))
(define-syntax-rule (ctor-pred c)
  @${@(stringify c)@textbf{?}})
(define-syntax record-gete
  (syntax-rules ()
    [(_ e ɐ) @${@(stringify e).@(stringify ɐ)}]
    [(_ ɐ) @${.@(stringify ɐ)}]))
(define-syntax-rule (record-pred . f*)
  @${(@textbf{record?}\ @(stringify . f*))})
(define-syntax-rule (record-pred* . f*)
  @${(@textbf{record$\mathbf{*}$?}\ @(stringify . f*))});; TODO text vs math!!!
(define-syntax-rule (opwith rec a v)
  @list{@(stringify rec) @textbf[" with "] @(stringify a) = @(stringify v)})
(define-syntax-rule (opwithout rec a)
  @list{@(stringify rec) @textbf[" without "] @(stringify a)})
(define pe @${\mathit{pe}})
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
  @${\mathbf{(}@(add-between (list (stringify f+args) ...) "\\ ")\mathbf{)}})

(define-syntax num-e* (defop "num"))
(define-syntax num-v* (defvop "num"))
(define-syntax num-τ* (defop "Num"))
(define num-e @num-e*[n])
(define num-v @num-v*[n])
(define-syntax num-τ
  (syntax-parser
    [(_ n) #'@num-τ*[n]]
    [self:id #'@num-τ*[n]])) ;; n by default

(define-syntax null-v (defvop "null"))
(define-syntax null-e (defop "null"))
(define-syntax null-τ (defop "Null"))

(define-syntax true-e (defop "true"))
(define-syntax true-v (defvop "true"))
(define-syntax true-τ (defop "True"))

(define-syntax false-e (defop "false"))
(define-syntax false-v (defvop "false"))
(define-syntax false-τ (defop "False"))

(define-syntax un (defop "∪"))
(define-syntax ∩τ (defop "∩"))

(define-syntax-rule (f→ (from ...) R)
  @${(@(add-between (list @(stringify from) ...) "\\ ")\ @mathbm{→}\ @(stringify R))})
(define-syntax-rule (f* (from ... rest*) R)
  (f→ (from ... @${\ @mathbm{.}\ } @${@(stringify rest*)@mathbm{*}}) R))
(define-syntax-rule (f… (from ... polydot) R)
  (f→ (from ... @${\ @mathbm{.}\ } polydot) R))
(define-syntax (R stx)
  (syntax-case stx ()
    [(_ to φ⁺ φ⁻ o)
     #'@${@mathbm{❲}@(stringify to)
      \;@mathbm{;}\; @(stringify φ⁺) @mathbm{{/}} @(stringify φ⁻)
      \;@mathbm{;}\; @(stringify o)@mathbm{❳}}]
    [self (identifier? #'self)
     #'@${\mathrm{R}}]))

(define primop "p")

(define-syntax (consp stx)
  (syntax-case stx ()
    [(_ a b) #'@${(\mathit{cons}\ @(stringify a)\ @(stringify b)}]
    [self (identifier? #'self) #'@${\mathit{cons}}])) ;; cons primop
(define-syntax-rule (consv a b) @${⟨@(stringify a), @(stringify b)⟩})
(define-syntax listv (defvop "list"))
(define-syntax-rule (consτ a b)
  @${❬@(stringify a)@mathbm{,} @(stringify b)❭})
(define-syntax-rule (polydot τ α)
  @${@(stringify τ) \mathbf{…}_{@(stringify α)}})
(define-syntax-rule (polydotα α)
  @${@(stringify α) \mathbf{…}})
(define-syntax List…τ* (defop "List"))
(define-syntax-rule (List…τ τ α)
  @List…τ*[@polydot[τ α]])
(define-syntax Listτ (defop "List"))
(define-syntax Listofτ (defop "Listof"))
@;(define-syntax →Values (defop "Values"))
(define-syntax-rule (→Values v ...) (spaces (stringify v) ...))
(define @emptypath @${ϵ})
(define-syntax-rule (<: a b)
  @${⊢ @(stringify a) \mathrel{≤:} @(stringify b)})
@define[<:*]{@${{\mathrel{≤:}}}}
(define-syntax-rule (=: a b)
  @${⊢ @(stringify a) \mathrel{=:} @(stringify b)})
(define-syntax-rule (≠: a b)
  @${⊢ @(stringify a) \mathrel{≠:} @(stringify b)})
(define-syntax-rule (=:def a b)
  @${@(stringify a) \mathrel{≝} @(stringify b)})

(define-syntax-rule (<:R a b)
  @${⊢ @(stringify a) \mathrel{{≤:}_R} @(stringify b)})

@(define-syntax Γ
   (syntax-parser
     #:literals (+) #:datum-literals (⊢ Δ)
     [(_ {~and {~not +} more} ...
         {~optional {~seq + φ}}
         {~optional {~seq Δ Δ∪ ...}}
         ⊢ x τ φ⁺ φ⁻ o)
      (raise-syntax-error 'Γ "Use of the old gamma syntax" this-syntax)]
     [(_ {~and {~not +} {~not Δ} more} ...
         {~optional {~seq + φ}}
         {~optional {~seq Δ Δ∪ ...}}
         ⊢ x R)
      #`@${@(add-between (list "Γ" (stringify more) ...) ", ")
       ;
       @(add-between (list "Δ" @#,@(if (attribute Δ∪)
                                       #'{(stringify Δ∪) ...}
                                       #'{}))
                     "∪")
       @#,@(if (attribute φ) @list{+ @#'(stringify φ)} @list{}) ⊢
       @(stringify x) : @(stringify R)}]))
@(define-syntax subst
   (syntax-parser
     [(_ {~seq from {~literal ↦} to} ...
         (~and {~seq repeated ...}
               {~seq {~optional ({~literal repeated} . _)}}))
      #'@$["[" (add-between (list (list (stringify from) "↦" (stringify to))
                                  ...
                                  repeated
                                  ...)
                            "\\ ") "]"]]))
@(define-syntax substφo
   (syntax-parser
     [(_ from {~literal ↦} to)
      #'@$["{|}_{{" (stringify from) "}↦{" (stringify to) "}}"]]))

(define update @${\operatorname{update}})
(define applyfilter @${\operatorname{applyfilter}})
(define combinefilter @${\operatorname{combinefilter}})
(define no-overlap @${\operatorname{no-overlap}})
(define restrict @${\operatorname{restrict}})
(define remove @${\operatorname{remove}})
(define simplify* @${\operatorname{simplify}})
(define-syntax-rule (simplify τ) @${@simplify*(@(stringify τ))})
(define propagate⊥ @${\operatorname{propagate_⊥}})
(define loc @${\mathit{loc}})
(define (! . rest) @${\overline{@rest}})
(define metatrue @${\mathrm{true}})
(define metafalse @${\mathrm{false}})

(define carπ @${\mathrm{car}})
(define cdrπ @${\mathrm{cdr}})
(define forceπ @${\mathrm{force}})
(define Numberτ @${\mathbf{Number}})
(define-syntax promisee (defop "delay"))
(define-syntax forcee (defop "force"))
(define-syntax promiseτ (defop "Promise"))
(define-syntax promisev (defvop "promise"))
(define-syntax syme (defop "symbol"))
(define-syntax symτ (defop "Symbol"))
(define-syntax symv (defvop "symbol"))
(define Symbolτ @${\mathbf{Symbol}})
(define-syntax gensyme (defop "gensym"))
(define-syntax eq?op (defop "eq?"))
(define sym* @${s′})
(define 𝒮* @${𝒮′})
(define-syntax recτ* (defop "Rec"))
(define-syntax-rule (recτ r τ) (recτ* r τ))
(define Booleanτ @${\mathbf{Boolean}})
(define (transdots a b c) @${\mathit{td_τ}(@a,\ @b,\ @c)})
(define (substdots a b c d) @${\mathit{sd}(@a,\ @b,\ @c,\ @d)})
(define object @emph{object})
(define Objects @emph{Objects})
(define (elim a b) @${\mathit{elim}(@a,\ @b)})
(define-syntax-rule (<:elim r a b)
  @${⊢ @(stringify a)
 \mathrel{{≤:}_{\mathrm{elim}\ @(stringify r)}}
 @(stringify b)})

(define-syntax include-equation
  (syntax-rules ()
    [(_ filename)
     (let ()
       (local-require (only-in (submod filename equations) [equations tmp]))
       tmp)]
    [(_ filename eq)
     (let ()
       (local-require (only-in filename [eq tmp]))
       tmp)]))

(define δe @${δ_{\mathrm{e}}})
(define alldifferent @${\operatorname{AllDifferent}})
(define disjoint-sets @${\operatorname{DisjointSets}})