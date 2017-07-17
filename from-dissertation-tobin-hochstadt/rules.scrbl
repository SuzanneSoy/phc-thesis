#lang scribble/manual

@; This file is not under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

The following definitions and rules are copied and adjusted
from@~cite["tobin-hochstadt_typed_2010"], with the author's permission. Some
of the notations were changed to use those of@~cite["kent2016occurrence"]

@require["../scribblings/util.rkt"
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
       @acase{x}
       @acase{@num-e}
       @acase{@true-e}
       @acase{@false-e}
       @acase{@primop}
       @acase{@app[e @repeated{e}]}
       @acase{@ifop[e e e]}
       @acase{@λe[(@repeated{x:τ}) e]}
       @acase{@λe[(@repeated{x:τ} @${\ .\ } @${x:τ*}) e]}
       @acase{@λe[(@repeated{x:τ} @${\ .\ } @${x:@polydot[τ α]}) e]}
       @acase{@Λe[(@repeated{α}) e]}
       @acase{@Λe[(@repeated{α} @polydotα[α]) e]}
       @acase{@conse[e e]} @; TODO: shouldn't it be a primop?
       @acase{@at[e @repeated{τ}]}]

Primitive operations:

@cases[@primop #:first-sep "⩴"
       @acase{@textit{add1}} ;; only here as an example
       @acase{@textit{number?}} ;; probably used in some explanation
       @acase{@textit{cons?}}
       @acase{@textit{null?}}
       @acase{@textit{car}}
       @acase{@textit{cdr}}
       @acase{…}]

Values:

@cases["v" #:first-sep "⩴"
       @acase{c}
       @acase{@num-v}
       @acase{@true-v}
       @acase{@false-v}
       @acase{@primop}
       @acase{@λv[ℰ (@repeated{x:τ}) e]}
       @acase{@λv[ℰ (@repeated{x:τ} @${\ .\ } @${x:τ*}) e]}
       @acase{@λv[ℰ (@repeated{x:τ} @${\ .\ } @${x:@polydot[τ α]}) e]}
       @acase{@Λv[ℰ (@repeated{α}) e]}
       @acase{@Λv[ℰ (@repeated{α} @polydotα[α]) e]}
       @acase{@consv[v v]}]

Execution environment:

@cases["ℰ" #:first-sep "⩴"
       @acase{@repeated{@↦v[x v]}\ @repeated{@↦v[α τ]}}]

Evaluation context:

@cases["E" #:first-sep "⩴"
       @acase{[]}
       @acase{@app[E @repeated{e}]}
       @acase{@app[v @repeated{v} E @repeated{e}]}
       @acase{@ifop[E e e]}
       @acase{@conse[E e]} @; TODO: shouldn't it be a primop?
       @acase{@conse[v E]}] @; TODO: shouldn't it be a primop?
        
@; TODO: are other cases needed?

Typing judgement:

@$${
 @Γ[⊢ e τ φ⁺ φ⁻ o]
}

@todo{Explain…}

Types:

@cases["τ,σ" #:first-sep "⩴"
       @acase{⊤}
       @acase{@num-τ}
       @acase{@true-τ}
       @acase{@false-τ}
       @acase{@f→[(@repeated{τ}) R]}
       @acase{@f→[(@repeated{τ} @${\ .\ } @${τ*}) R]}
       @acase{@f→[(@repeated{τ} @${\ .\ } @polydot[τ α]) R]}
       @acase{@∀r[@${(@repeated{α})} @repeated{τ}]}
       @acase{@∀r[@${(@repeated{α} @polydotα[α])} @repeated{τ}]}
       @acase{@un[@repeated{τ}]}
       @acase{@consτ[τ τ]}]

@cases[@R #:first-sep "⩴"
       @R[@→Values[@repeated{τ}]
          @${φ⁺}
          @${φ⁻}
          @${o}]]


Filters (conditional typing information):

@cases[@${φ} #:first-sep "⩴" @acase{@repeatset{ψ}}]

@cases["ψ" #:first-sep "⩴"
       @acase{τₓ}
       @acase{\overline{τ}ₓ}
       @acase{⊥}]

Objects (aliasing information):

@cases[@textrm{o} #:first-sep "⩴"
       @acase{π}
       @acase{∅}]

Paths:

@cases[@textit{π} #:first-sep "⩴"
       @acase{pe∷π}
       @acase{@emptypath \qquad\qquad\text{empty path}}]

The path concatenation operator @${∷} is associative. @htodo{Actually, we
 define it for pe∷π above, not for π∷π}. The @${@emptypath} is omitted from
paths with one or more elements, so we write @${car∷cdr} instead of @${
 car∷cdr∷@emptypath}.

Path elements (aliasing information):

@cases[@textit{pe} #:first-sep "⩴"
       @acase{car}
       @acase{cdr}]

Subtyping:

@$inferrule[
 @${}
 @${⊢ τ <: τ}
 @${@textsc{S-Refl}}
 ]
  
@$inferrule[
 @${}
 @${⊢ τ <: ⊤}
 @${@textsc{S-Top}}
 ]

@textsc{S-Bot} can be derived from @textsc{S-UnionSub}, by constructing an
empty union. The @${⊥} type is a shorthand for @${(∪)}. It is a subtype of
every other type, and is not inhabited by any value.

@$inferrule[
 @${}
 @${⊢ ⊥ <: τ}
 @${@textsc{S-Bot}}
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
  @repeated{@<:[τ_r σ_r]} \\
  @${φ⁺' ⊆ φ⁺ } \\
  @${φ⁻' ⊆ φ⁻ } \\
  o = o' ∨ o' = ∅}
 @${@<:R[@R[@→Values[@repeated{τ_r}]
            @${φ⁺}
            @${φ⁻}
            @${o}]
         @R[@→Values[@repeated{σ_r}]
            @${φ'⁺}
            @${φ'⁻}
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

@$inferrule[@${}
            @${@Γ[⊢ x @${Γ(x)} @${\overline{@false-τ}} @true-τ x]}
            @${@textsc{T-Var}}]

@$inferrule[@${}
            @${@Γ[⊢ p @${δ_τ(p)} ϵ ⊥ ∅]}
            @${@textsc{T-Primop}}]

@$inferrule[@${}
            @${@Γ[⊢ @truev @true-τ ϵ ⊥ ∅]}
            @${@textsc{T-True}}]

@$inferrule[@${}
            @${@Γ[⊢ @falsev @false-τ ⊥ ϵ ∅]}
            @${@textsc{T-False}}]

@$inferrule[@${}
            @${@Γ[⊢ @num-v @num-τ ϵ ⊥ ∅]}
            @${@textsc{T-Num}}]


TODO: Drop the object and filters which are not in terms of x₀, rename
otherwise.

@$inferrule[@${@Γ[@repeated{x:σ} ⊢ e τ φ⁺ φ⁻ o]}
            @${@Γ[⊢ @λe[(@repeated{x:σ}) e]
                  (f→ (@repeated{σ})
                      @R[@→Values[τ]
                         @${φ⁺[x ↦ ?]}
                         @${φ⁻[x ↦ ?]}
                         @${o[x ↦ ?]}])
                  ϵ ⊥ ∅]}
            @${@textsc{T-Abs}}]