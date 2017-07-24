#lang scribble/manual

@; This file is not under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

The following definitions and rules are copied and adjusted
from@~cite["tobin-hochstadt_typed_2010"], with the author's permission. Some
of the notations were changed to use those of@~cite["kent2016occurrence"]. We
include below the grammar, semantics and typing rules related to the minimal
core of the Typed Racket language@note{The core language is defined
 in@~cite[#:precision "pp. 61–70" "tobin-hochstadt_typed_2010"].}, dubbed @${
 λ_{\mathit{TS}}}, including extensions which add pairs@note{The extensions
 needed to handle pairs are described
 in@~cite[#:precision "pp. 71–75" "tobin-hochstadt_typed_2010"].}, functions of
multiple arguments, variadic functions and variadic polymorphic
functions@note{The extensions needed to handle functions of multiple
 arguments, variadic functions, and variadic functions where the type of the
 “rest” arguments are not uniform are described
 in@~cite[#:precision "pp. 91–77" "tobin-hochstadt_typed_2010"].}, @todo{
 intersection types}, @todo{recursive types}, @todo{symbols} and @todo{
 promises}. We purposefully omit extensions which allow advanced logic
reasoning when propagating information gained by complex combinations of
conditionals@note{The extensions which allow advanced logic reasoning are
 described in@~cite[#:precision "pp. 75–78" "tobin-hochstadt_typed_2010"].},
refinement types@note{The extensions which introduce refinement types are
 described in@~cite[#:precision "pp. 85–89" "tobin-hochstadt_typed_2010"].},
dependent refinement types@note{Dependent refinement types are presented in
 @~cite["kent2016occurrence"].} (which allow using theories from external
solvers to reason about values and their type, e.g. using bitvector theory to
ensure that a sequence of operations does not produce a result exceeding a
certain machine integer size), structs and classes. These extensions are not
relevant to our work@note{We informally describe a translation of our system
 of records into structs in section @todo{[??]}, but settle for an alternative
 implementation in section @todo{[??]} which does not rely on structs.}, and
their inclusion in the following semantics would needlessly complicate things.

@require["../scribblings/util.rkt"
         "../scribblings/abbreviations.rkt"
         "../scribblings/adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket)
                    typed/racket/class)
         scribble/example
         racket/string]
@(use-mathjax)

@(define tr-eval (make-eval-factory '(typed/racket)))

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)
       #:tag "from-dissertation-tobin-hochstadt"]{Formal semantics for part of
 @|typedracket|'s type system}

Expressions:

@cases["e" #:first-sep "⩴"
       @acase{x @P y @P z@tag*{variable}}
       @acase{@num-e @tag*{number}}
       @acase{@true-e @tag*{booleans}}
       @acase{@false-e}
       @acase{@null-e @tag*{null constant}}
       @acase{@primop @tag*{primitive functions}}
       @acase{@app[e @repeated{e}] @tag*{function application}}
       @acase{@ifop[e e e] @tag*{conditional}}
       @acase{@λe[(@repeated{x:τ}) e] @tag*{lambda function}}
       @acase{@λe[(@repeated{x:τ} @${\ .\ } @${x:τ*}) e]
        @tag*{variadic function}}
       @acase{@λe[(@repeated{x:τ} @${\ .\ } @${x:@polydot[τ α]}) e]
        @tag*{variadic polymorpic function}}
       @acase{@Λe[(@repeated{α}) e]@tag*{polymorphic abstraction}}
       @acase{@Λe[(@repeated{α} @polydotα[α]) e]
        @tag*{variadic polymorphic abstraction}}
       @acase{@at[e @repeated{τ}] @tag*{polymorphic instantiation}}
       @acase{@conse[e e]@tag*{pair}}] @; TODO: shouldn't it be a primop?

Primitive operations:

@cases[@primop #:first-sep "⩴"
       @acase{@textit{add1}@tag*{returns its argument plus @${1}}} ;; only here as an example
       @acase{@textit{number?}@tag*{number predicate}} ;; probably used in some explanation
       @acase{@textit{cons?}@tag*{pair predicate}}
       @acase{@textit{null?}@tag*{@null-v predicate}}
       @acase{@textit{car}@tag*{first element of pair}}
       @acase{@textit{cdr}@tag*{second element of pair}}
       @acase{…}]

Values:

@cases["v" #:first-sep "⩴"
       @acase{@primop @tag*{primitive function}}
       @acase{@num-v @tag*{number}}
       @acase{@true-v @tag*{booleans}}
       @acase{@false-v}
       @acase{@λv[ℰ (@repeated{x:τ}) e] @tag*{lambda function}}
       @acase{@λv[ℰ (@repeated{x:τ} @${\ .\ } @${x:τ*}) e]
        @tag*{variadic function}}
       @acase{@λv[ℰ (@repeated{x:τ} @${\ .\ } @${x:@polydot[τ α]}) e]
        @tag*{variadic polymorphic function}}
       @acase{@Λv[ℰ (@repeated{α}) e]
       @tag*{polymorphic abstraction}}
       @acase{@Λv[ℰ (@repeated{α} @polydotα[α]) e]
       @tag*{variadic polymorphic abstraction}}
       @acase{@consv[v v] @tag*{pair}}
       @acase{@null-v @tag*{null}}]

Execution environment:

@cases["ℰ" #:first-sep "⩴"
       @acase{@repeated{@↦v[x v]}\ @repeated{@↦v[α τ]}
        @tag*{bound variables @${\&} types}}]

Evaluation context:

@cases["E" #:first-sep "⩴"
       @acase{[] @tag*{program entry point}}
       @acase{@app[E @repeated{e}]@tag*{function application}}
       @acase{@app[v @repeated{v} E @repeated{e}]}
       @acase{@ifop[E e e]@tag*{conditional}}
       @acase{@conse[E e]@tag*{pair}} @; TODO: shouldn't it be a primop?
       @acase{@conse[v E]}] @; TODO: shouldn't it be a primop?
        
@; TODO: are other cases needed?

Typing judgement:

@$${
 @Γ[⊢ e R]
}

@cases[@R #:first-sep "⩴"
       @R[τ
          @${φ⁺}
          @${φ⁻}
          @${o}]]

The @Γ[⊢ e R] typing judgement indicates that the expression @${e} has type
@${τ}.

Additionally, the typing judgement indicates a set of propositions @${φ⁻}
which are known to be true when the run-time value of @${e} is @|false-v|, and
a set of propositions @${φ⁺} which are known to be true when the run-time
value of @${e} is @|true-v|@note{Any other value is treated in the same way as
 @|true-v|, as values other than @|false-v| are traditionally considered as
 true in language of the @lisp family}. The propositions will indicate that the
value of a separate variable belongs (or does not belong) to a given type. For
example, the @${φ⁻} proposition @${Number_y} indicates that when @${e}
evaluates to @|false-v|, the variable @${y} necessarily holds an integer.

Finally, the typing judgement can indicate with @${o} that the expression @${
 e} is an alias for a sub-element of another variable in the environment. For
example, if the object @${o} is @${@carπ ∷ @cdrπ(y)}, it indicates that the
expression @${e} produces the same value that @racket[(car (cdr y))] would,
i.e. that it returns the second element of a (possibly improper) list stored
in @racket[y].

Readers familiar with abstract interpretation can compare the @${φ}
propositions to the Cartesian product of the abstract domains of pairs of
variables. A static analyser can track possible pairs of values contained in
pairs of distinct variables, and will represent this information using an
abstract domain which combinations of values may be possible, and which may
not. Occurrence typing similarly exploits the fact that the type of other
variables may depend on the value of @${τ}. @htodo{is this some weak form of
 dependent typing?}

Types:

@cases["τ,σ" #:first-sep "⩴"
       @acase{⊤@tag*{top}}
       @acase{@num-τ @tag*{number singleton}}
       @acase{@Numberτ @tag*{any number}}
       @acase{@true-τ @tag*{boolean singleton}}
       @acase{@false-τ}
       @acase{@f→[(@repeated{τ}) R] @tag*{function}}
       @acase{@f→[(@repeated{τ} @${\ .\ } @${τ*}) R] @tag*{variadic function}}
       @acase{@f→[(@repeated{τ} @${\ .\ } @polydot[τ α]) R]
        @tag*{variadic polymorphic function}}
       @acase{@∀r[@${(@repeated{α})} @repeated{τ}]@tag*{polymorphic type}}
       @acase{@∀r[@${(@repeated{α} @polydotα[α])} @repeated{τ}]
        @tag*{variadic polymorphic type}}
       @acase{@un[@repeated{τ}]@tag*{union}}
       @acase{@consτ[τ τ]@tag*{pair}}
       @acase{@null-τ @tag*{null (end of lists)}}]

@htodo{Add the rec types}

Filters (a.k.a. propositions):

@cases[@${φ} #:first-sep "⩴" @acase{@repeatset{ψ}@tag*{filter set}}]

@cases["ψ" #:first-sep "⩴"
       @acase{τ_{@loc}
        @tag*{@${ℰ[v] = \mathbf{?} ⇒ ℰ[@loc]@text{ is of type @${τ}}}}}
       @acase{@!{τ}_{@loc}
        @tag*{@${ℰ[v] = \mathbf{?} ⇒ ℰ[@loc]@text{ is not of type @${τ}}}}}
       @acase{⊥@tag*{contradiction}}]

@cases[@loc #:first-sep "⩴"
       @acase{•@tag*{function's first argument}}
       @acase{x@tag*{variable}}]

Objects (aliasing information):

@cases[@textrm{o} #:first-sep "⩴"
       @acase{π(@loc)@tag*{@${e} is an alias for @${π(@loc)}}}
       @acase{∅@tag*{no aliasing information}}]

Paths:

@cases[@textit{π} #:first-sep "⩴"
       @acase{pe∷π@tag*{path concatenation}}
       @acase{@emptypath @tag*{empty path}}]

The path concatenation operator @${∷} is associative. @htodo{Actually, we
 define it for pe∷π above, not for π∷π}. The @${@emptypath} is omitted from
paths with one or more elements, so we write @${car∷cdr} instead of @${
 car∷cdr∷@emptypath}.

Path elements (aliasing information):

@cases[@textit{pe} #:first-sep "⩴"
       @acase{@carπ @tag*{first element of pair}}
       @acase{@cdrπ @tag*{second element of pair}}]

Subtyping:

@$inferrule[
 -
 @${⊢ τ <: τ}
 @${@textsc{S-Refl}}
 ]
  
@$inferrule[
 -
 @${⊢ τ <: ⊤}
 @${@textsc{S-Top}}
 ]

@textsc{S-Bot} can be derived from @textsc{S-UnionSub}, by constructing an
empty union. The @${⊥} type is a shorthand for @${(∪)}. It is a subtype of
every other type, and is not inhabited by any value.

@$inferrule[
 -
 @${⊢ ⊥ <: τ}
 @${@textsc{S-Bot}}
 ]

@$inferrule[
 -
 @${⊢ @num-τ <: @Numberτ}
 @${@textsc{S-Number}}
 ]

@$inferrule[
 @${
  @repeated{@<:[σ_a τ_a]} \\
  @<:R[R @${@R'}]}
 @${@<:[@f→[(@repeated{τ_a}) R]
        @f→[(@repeated{σ_a}) @${@R'}]]}
 @${@textsc{S-Fun}}
 ]

@$inferrule[
 @${
  @<:[τ_r σ_r] \\
  @${φ⁺' ⊆ φ⁺ } \\
  @${φ⁻' ⊆ φ⁻ } \\
  o = o' ∨ o' = ∅}
 @${@<:R[@R[τ_r
            @${φ⁺}
            @${φ⁻}
            @${o}]
         @R[σ_r
            @${φ⁺'}
            @${φ⁻'}
            @${o'}]]}
 @${@textsc{S-R}}
 ]

@$inferrule[
 @${
  @repeated{@<:[σ_a τ_a]} \\
  @<:[σ τ] \\
  @<:R[R @${@R'}]}
 @${@<:[@f→[(@repeated{τ_a} @${\ .\ } τ*) R]
        @f→[(@repeated{σ_a} @${\ .\ } σ*) @${@R'}]]}
 @${@textsc{S-Fun*}}
 ]

@$inferrule[
 @${
  @repeated{@<:[σ_a τ_a]} \\
  @repeated{@<:[σᵢ τ]} \\
  @<:R[R @${@R'}]}
 @${@<:[@f→[(@repeated{τ_a} @${\ .\ } τ*) R]
        @f→[(@repeated{σ_a} @repeated{σᵢ}) @${@R'}]]}
 @${@textsc{S-Fun*-Fixed}}
 ]

@$inferrule[
 @${
  @repeated{@<:[σ_a τ_a]} \\
  @repeated{@<:[σᵢ τ]} \\
  @<:[σ τ] \\
  @<:R[R @${@R'}]}
 @${@<:[@f→[(@repeated{τ_a} @${\ .\ } τ*) R]
        @f→[(@repeated{σ_a} @repeated{σᵢ} @${\ .\ } σ*) @${@R'}]]}
 @${@textsc{S-Fun*-Fixed*}}
 ]

@$inferrule[
 @${
  @repeated{@<:[σ_a τ_a]} \\
  @<:[σ τ] \\
  @<:R[R @${@R'}]}
 @${@<:[@f→[(@repeated{τ_a} @${\ .\ } @polydot[τ α]) R]
        @f→[(@repeated{σ_a} @${\ .\ } @polydot[σ α]) @${@R'}]]}
 @${@textsc{S-DFun}}
 ]

@$inferrule[
 @${@<:[@${τ[@repeated{αᵢ ↦ βᵢ}]} σ]}
 @${@<:[@∀r[@${(@repeated{αᵢ})} τ]
        @∀r[@${(@repeated{βᵢ})} σ]]}
 @${@textsc{S-Poly-}α@textsc{-Equiv}}
 ]

@todo{This should use the substitution for a polydot (subst-dots?), not the
 usual subst.}

@$inferrule[
 @${@<:[@${τ[@repeated{αᵢ ↦ βᵢ} α ↦ β]} σ]}
 @${@<:[@∀r[@${(@repeated{αᵢ} @polydotα[α])} τ]
        @∀r[@${(@repeated{βᵢ} @polydotα[β])} σ]]}
 @${@textsc{S-PolyD-}α@textsc{-Equiv}}
 ]

@todo{check the following rule:}

@htodo{Try to detach the ∀ from the →, in case the → is nested further deep.
 If it works.}

@$inferrule[
 @${
  @repeated{@<:[σ_a τ_a]} \\
  @<:[σ @${τ[α ↦ ⊤]}] \\
  @<:R[R @${@R'}]}
 @${@<:[@∀r[@${(@polydotα[α])} @f→[(@repeated{τ_a} @polydot[τ α]) R]]
        @f→[(@repeated{σ_a} σ*) @${@R'}]]}
 @${@textsc{S-DFun-Fun*}}
 ]

@todo{What if some tvars are unbound in the types above, how do they compare?}

@$inferrule[
 @${∃ i . @<:[τ @${σᵢ}]}
 @${@<:[τ @${\bigcup @repeated{σᵢ}}]}
 @${@textsc{S-UnionSuper}}
 ]

@$inferrule[
 @${@repeated[@<:[τᵢ @${σ}]]}
 @${@<:[@${\bigcup @repeated{τᵢ}} σ]}
 @${@textsc{S-UnionSub}}
 ]

@$inferrule[
 @${@<:[τ₁ σ₁] \\
  @<:[τ₂ σ₂]}
 @${@<:[@consτ[τ₁ τ₂] @consτ[σ₁ σ₂]]}
 @${@textsc{S-Pair}}
 ]

Operational semantics:



Typing rules:

@$inferrule[-
            @${@Γ[⊢ x @R[@${Γ(x)} @${@!{@false-τ}} @true-τ x]]}
            @${@textsc{T-Var}}]

@$inferrule[-
            @${@Γ[⊢ p @R[@${δ_τ(p)} ϵ ⊥ ∅]]}
            @${@textsc{T-Primop}}]

@$inferrule[-
            @${@Γ[⊢ @true-e @R[@true-τ ϵ ⊥ ∅]]}
            @${@textsc{T-True}}]

@$inferrule[-
            @${@Γ[⊢ @false-e @R[@false-τ ⊥ ϵ ∅]]}
            @${@textsc{T-False}}]

@$inferrule[-
            @${@Γ[⊢ @num-e @R[@num-τ ϵ ⊥ ∅]]}
            @${@textsc{T-Num}}]

@$inferrule[-
            @${@Γ[⊢ @null-e @R[@null-τ ⊥ ϵ ∅]]}
            @${@textsc{T-Null}}]


@$inferrule[@${@Γ[@${x₀:σ₀} @repeated{xᵢ:σ} ⊢ e @R[τ φ⁺ φ⁻ o]] \\
            φ⁺' = φ⁺\vphantom{φ}@substφo[x₀ ↦ •] \\
            φ⁻' = φ⁻\vphantom{φ}@substφo[x₀ ↦ •] \\
            o' = o\vphantom{o}@substφo[x₀ ↦ •]}
            @${@Γ[⊢ @λe[(@repeated{x:σ}) e]
                  @R[(f→ (@repeated{σ})
                         @R[τ
                            @${φ⁺'}
                            @${φ⁻'}
                            @${o'}])
                     ϵ ⊥ ∅]]}
            @${@textsc{T-AbsPred}}]

@$inferrule[@${@Γ[⊢ e @R[τ φ⁺ φ⁻ o]]}
            @${@Γ[⊢ @λe[(@repeated{x:σ}) e]
                  @R[(f→ (@repeated{σ})
                         @R[τ
                            ϵ
                            ϵ
                            ∅])
                     ϵ ⊥ ∅]]}
            @${@textsc{T-Abs}}]

The @${\vphantom{φ}@substφo[x ↦ z]} operation restricts the information
contained within a @${φ} or @${o} so that the result only contains information
about the variable @${x}, and renames it to @${z}. When applied to a filter
@${φ}, it corresponds to the @${\operatorname{abo}} and @${\operatorname{
  apo}} operators from
@~cite[#:precision "pp. 65,75" "tobin-hochstadt_typed_2010"].

The @${⊥} cases of the @${\operatorname{apo}} operator
from@~cite[#:precision "pp. 65,75" "tobin-hochstadt_typed_2010"] are covered
by the corresponding cases in the @${@restrict} and @${@remove} operators, and
therefore should not need to be included in our @${\vphantom{
  φ}@substφo[x ↦ z]} operator.

@$${
 \begin{aligned}
 φ@substφo[x ↦ z] &= \bigcup @repeated{ψ@substφo[x ↦ z]}&\\
 ⊥@substφo[x ↦ z] &= \{⊥\}&\\
 τ_{π(y)}\vphantom{τ}@substφo[x ↦ z] &= ∅ &@textif y ≠ x \\
 @!{τ}_{π(y)}\vphantom{τ}@substφo[x ↦ z] &= ∅ &@textif y ≠ x \\
 τ_{π(x)}\vphantom{τ}@substφo[x ↦ z] &= \{τ_{π(z)}\} &\\
 @!{τ}_{π(x)}\vphantom{τ}@substφo[x ↦ z] &= \{@!{τ}_{π(z)}\} &
 \end{aligned}
}

@$${
 \begin{aligned}
 π(x)@substφo[x ↦ ∅] &= ∅ &\\
 π(x)@substφo[x ↦ z] &= π(z) &@textif z ≠ ∅ \\
 π(y)@substφo[x ↦ z] &= ∅ &@textif y ≠ x \\
 ∅@substφo[x ↦ z] &= ∅ &
 \end{aligned}
}

@(define _op @${_{\mathit{op}}})

@$inferrule[@${@Γ[⊢ @${e@_op} @R[@${τ@_op} @${φ⁺@_op} @${φ⁻@_op} @${o@_op}]] \\
             @repeated[@Γ[⊢ @${aᵢ}
                          @R[@${τ_{aᵢ}} @${φ⁺_{aᵢ}} @${φ⁻_{aᵢ}} @${o_{aᵢ}}]]] \\
             @repeated[@<:[τ_a @${τ_{\mathit{in}}}]]
             @<:[@${τ@_op} @f→[(@repeated{τ_{\mathit{in}}})
                               @R[τ_r φ⁺_r φ⁻_r o_r]]]
            φ⁺_r' = φ⁺_r@substφo[• ↦ @${o_{a₀}}] \\
            φ⁻_r' = φ⁻_r@substφo[• ↦ @${o_{a₀}}] \\
            o' = o@substφo[• ↦ @${o_{a₀}}]}
            @${@Γ[⊢ @app[@${e@_op} @repeated{aᵢ}]
                  @R[(f→ (@repeated{σ})
                         @R[τ_r
                            @${φ⁺'}
                            @${φ⁻'}
                            @${o'}])
                     ϵ ⊥ ∅]]}
            @${@textsc{T-App}}]

@$inferrule[@${@Γ[⊢ @${e₁} @R[@${τ₁} @${φ⁺₁} @${φ⁻₁} @${o₁}]] \\
             @Γ[+ φ⁺₁ ⊢ @${e₂} @R[@${τ₂} @${φ⁺₂} @${φ⁻₂} @${o₂}]] \\
             @Γ[+ φ⁻₁ ⊢ @${e₃} @R[@${τ₃} @${φ⁺₃} @${φ⁻₃} @${o₃}]] \\
             @<:[τ₂ τ_r] \\
             @<:[τ₃ τ_r] \\
             φ_r = @combinefilter(φ⁺₁ / φ⁻₁, φ⁺₂ / φ⁻₂, φ⁺₃ / φ⁻₃) \\
             o_r = \begin{cases}
             o₂ @& @textif o₂ = o₃
             @nl ∅ @& @otherwise
             \end{cases}
             }
            @${@Γ[⊢ @ifop[e₁ e₂ e₃] @R[τ_r ϵ ⊥ ∅]]}
            @${@textsc{T-If}}]

@htodo{The definition of Γ' does not specify what the other cases ≠ x are
 (they are the same as the original Γ, but this is only implicit).}

@aligned{
 Γ + \{τ_{π(x)}\} ∪ @repeatset{ψ}
 &= (Γ, x : @update(Γ(x), τ_π)) + @repeatset{ψ}\\
 Γ + \{@!{τ}_{π(x)}\} ∪ @repeatset{ψ}
 &= (Γ, x : @update(Γ(x), @!{τ}_π)) + @repeatset{ψ}\\
 Γ + \{⊥\} ∪ @repeatset{ψ} &= Γ' @where ∀x∈ \operatorname{dom}(Γ).Γ'(x) = ⊥\\
 Γ + ϵ &= Γ \\
}

@aligned{
 @update(@consτ[τ τ′], σ_{π∷car} )
 &= @consτ[@${@update(τ, σ_π)} τ′]\\
 @update(@consτ[τ τ′], @!{σ}_{π∷car})
 &= @consτ[@${@update(τ, @!{σ}_π)} τ′]\\
 @update(@consτ[τ τ′], σ_{π∷cdr} )
 &= @consτ[τ @${@update(τ′, σ_π)}]\\
 @update(@consτ[τ τ′], @!{σ}_{π∷cdr} )
 &= @consτ[τ @${@update(τ′, @!{σ}_π)}]\\
 @update(τ, σ_ϵ) &= @restrict(τ, σ) \\
 @update(τ, @!{σ}_ϵ) &= @remove(τ, σ)
}

@aligned{
 @restrict(τ, σ) &= ⊥ &@textif @no-overlap(τ,σ)\\
 @restrict((⋃ @repeatset{τ}), σ) &= (⋃ @repeatset{@restrict(τ,σ)} &\\
 @restrict(τ, σ) &= τ &@textif @<:[τ σ]\\
 @restrict(τ, σ) &= σ &@otherwise
}

@aligned{
 @remove(τ, σ) &= ⊥ &@textif @<:[τ σ] \\
 @remove((⋃ @repeatset{τ}), σ) &= (⋃ @repeatset{@remove(τ,σ)} &\\
 @remove(τ, σ) &= τ &@otherwise
}

@todo{Shouldn't no-overlap be simplified to @${@no-overlap(τ, τ') = (@<:[σ τ]
  ∧ @<:[σ τ′] ⇒ σ = ⊥)}? Then @${@restrict(τ,σ)} can be simplified to returning
 the most general type which is a subtype of τ and σ if one exists (or maybe
 simply returning the intersection of τ and σ).}

@aligned{
 @no-overlap(τ, τ′) &= @metatrue
 &&@textif (∃! σ . \quad @<:[σ τ]\quad ∧ \quad@<:[σ τ′]) ∧ σ = ⊥\\
 @no-overlap(τ, σ) &= @metafalse
 &&@otherwise
}

@;{
 @aligned{
  @no-overlap(@num-τ[n], @num-τ[m]) &= @metatrue @textif n ≠ m \\
  @no-overlap(@num-τ, @true-τ) &= @metatrue \\
  @no-overlap(@num-τ, @false-τ) &= @metatrue \\
  @no-overlap(@num-τ, @null-τ) &= @metatrue \\
  @no-overlap(@Numberτ, @true-τ) &= @metatrue \\
  @no-overlap(@Numberτ, @false-τ) &= @metatrue \\
  @no-overlap(@Numberτ, @null-τ) &= @metatrue \\
  @no-overlap(@true-τ, @false-τ) &= @metatrue \\
  @no-overlap(@true-τ, @null-τ) &= @metatrue \\
  @no-overlap(@false-τ, @null-τ) &= @metatrue \\
  @no-overlap(@num-τ, @f→[(@repeated{τ}) @R]) &= @metatrue \\
  @no-overlap(@Numberτ, @f→[(@repeated{τ}) @R]) &= @metatrue \\
  @no-overlap(@true-τ, @f→[(@repeated{τ}) @R]) &= @metatrue \\
  @no-overlap(@false-τ, @f→[(@repeated{τ}) @R]) &= @metatrue \\
  @no-overlap(@consτ[τ τ′], @f→[(@repeated{τ}) @R]) &= @metatrue \\
  @no-overlap(@null-τ, @f→[(@repeated{τ}) @R]) &= @metatrue \\
  @no-overlap(@num-τ, @consτ[τ τ′]) &= @metatrue \\
  @no-overlap(@Numberτ, @consτ[τ τ′]) &= @metatrue \\
  @no-overlap(@true-τ, @consτ[τ τ′]) &= @metatrue \\
  @no-overlap(@false-τ, @consτ[τ τ′]) &= @metatrue \\
  @no-overlap(@null-τ, @consτ[τ τ′]) &= @metatrue \\
  @no-overlap(@consτ[τ τ′], @consτ[σ σ′])
  &= @no-overlap(τ,σ) ∨ @no-overlap(τ′,σ′)\\
  @no-overlap((⋃ @repeatset{τ}), σ) &= ⋀@repeated{@no-overlap(τ,σ)}\\
  @no-overlap(τ, σ) &= @metatrue @textif @no-overlap(σ, τ)\\
  @no-overlap(τ, σ) &= @metafalse @otherwise \\
 }
}

@htodo{Say that there are more rules in the implementation, to handle various
 boolean operations.}

@aligned{
 @combinefilter(ϵ / ⊥, φ^±₂, φ^±₃) &= φ₂ &\\
 @combinefilter(⊥ / ϵ, φ^±₂, φ^±₃) &= φ₃ &\\
 @combinefilter(⊥ / ⊥, φ^±₂, φ^±₃) &= ⊥ &\\
 @combinefilter(φ⁺₁ / φ⁻₁, φ⁺₂ / φ⁻₂, ⊥/ϵ) &= φ⁺₁ ∪ φ⁺₂ &\\
 @combinefilter(
 \{ τ_@loc \} ∪ φ⁺₁ / \{ @!{τ}_@loc \} φ⁻₁,
 ϵ / ⊥,
 ⊥/ϵ)
 &= (∪ τ σ)_@loc / @!{(∪ τ σ)_@loc} &\\
 … & = … & \\
 @combinefilter(⊥ / ⊥, φ^±₂, φ^±₃) &= ϵ / ϵ &@otherwise \\
}

@htodo{The Γ ⊢ x : τ … does not generate a Γ(x) = τ, I suspect. There should
 be indicated somewhere an equivalence between these two notations (and we
 should fix the @${Γ,x:update(…)}, as it is a third notation).}