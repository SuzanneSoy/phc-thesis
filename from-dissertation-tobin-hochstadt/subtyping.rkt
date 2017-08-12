#lang at-exp s-exp phc-thesis/scribblings/equations-lang

@; This file is NOT under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

#:S-Reflexive

@$inferrule[
 -
 @${@<:[τ τ]}
 @${@textsc{S-Reflexive}}]

#:S-Transitive

@$inferrule[
 @${@<:[τ τ′] \\ @<:[τ′ τ″]}
 @${@<:[τ τ″]}
 @${@textsc{S-Transitive}}]

#:S-Top

@$inferrule[
 -
 @${@<:[τ ⊤]}
 @${@textsc{S-Top}}]

#:S-Bot

@$inferrule[
 -
 @${@<:[⊥ τ]}
 @${@textsc{S-Bot}}]

#:S-Number

@$inferrule[
 -
 @${@<:[@num-τ @Numberτ]}
 @${@textsc{S-Number}}]

#:S-Symbol

@$inferrule[
 -
 @${@<:[@symτ[s] @Symbolτ]}
 @${@textsc{S-Symbol}}]

#:S-RecWrap

@$inferrule[@${@<:[τ σ]}
            @${@<:[@recτ[r τ] σ]}
            @${@textsc{S-RecWrap}}]

#:S-RecElim

@$inferrule[@${@<:[τ σ]}
            @${@<:[τ @recτ[r σ]]}
            @${@textsc{S-RecElim}}]

#:S-RecStep

;; This rule allows the following sort of subtyping:
;; This is necessary to build recursive functions which return Rec types,
;; by adding one "step" to the recursive construction of the value.
#;{ (Pairof Integer (Rec R (Pairof Integer R))) <: (Rec R (Pairof Integer R)) }


@$inferrule[-
            @${@<:[@${σ@subst[r ↦ @recτ[r σ]]} @recτ[r σ]]}
            @${@textsc{S-RecStep}}]

#:S-RecUnStep

@$inferrule[-
            @${@<:[@recτ[r σ] @${σ@subst[r ↦ @recτ[r σ]]}]}
            @${@textsc{S-RecUnStep}}]

@;{
 @$inferrule[-
             @${@<:[@elim[@${r} @${τ}] @recτ[r τ]]}
             @${@textsc{S-Rec}}]

 where

 @$${
  \begin{aligned}
  @elim[@${r} @un[@repeatset{τ′} r]] &= @un[@repeatset[@elim[@${r} @${τ′}]]]\\
  @elim[@${r} @${τ}]&\hphantom{{}={}}@text{is applied pointwise otherwise}
  \end{aligned}
 }
}

#:S-Fun

@$inferrule[@${@repeated{@<:[σ_a τ_a]} \\
             @<:R[R @${@R'}]}
            @${@<:[@f→[(@repeated{τ_a}) R]
                   @f→[(@repeated{σ_a}) @${@R'}]]}
            @${@textsc{S-Fun}}]

#:S-R

@$inferrule[@${@<:[τ_r σ_r] \\
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
            @${@textsc{S-R}}]

#:S-Fun*

@$inferrule[
 @${
  @repeated{@<:[σ_a τ_a]} \\
  @<:[σ τ] \\
  @<:R[R @${@R'}]}
 @${@<:[@f*[(@repeated{τ_a} τ*) R]
        @f*[(@repeated{σ_a} σ*) @${@R'}]]}
 @${@textsc{S-Fun*}}]

#:S-Fun*-Fixed

@$inferrule[
 @${
  @repeated{@<:[σ_a τ_a]} \\
  @repeated{@<:[σᵢ τ]} \\
  @<:R[R @${@R'}]}
 @${@<:[@f*[(@repeated{τ_a} τ*) R]
        @f→[(@repeated{σ_a} @repeated{σᵢ}) @${@R'}]]}
 @${@textsc{S-Fun*-Fixed}}]

#:S-Fun*-Fixed*

@$inferrule[
 @${
  @repeated{@<:[σ_a τ_a]} \\
  @repeated{@<:[σᵢ τ]} \\
  @<:[σ τ] \\
  @<:R[R @${@R'}]}
 @${@<:[@f*[(@repeated{τ_a} τ*) R]
        @f*[(@repeated{σ_a} @repeated{σᵢ} σ*) @${@R'}]]}
 @${@textsc{S-Fun*-Fixed*}}]

#:S-DFun

@$inferrule[
 @${
  @repeated{@<:[σ_a τ_a]} \\
  @<:[σ τ] \\
  @<:R[R @${@R'}]}
 @${@<:[@f…[(@repeated{τ_a} @polydot[τ α]) R]
        @f…[(@repeated{σ_a} @polydot[σ α]) @${@R'}]]}
 @${@textsc{S-DFun}}]

#:S-Poly-α-Equiv

@$inferrule[
 @${@<:[@${τ[@repeated{αᵢ ↦ βᵢ}]} σ]}
 @${@<:[@∀r[(@repeated{αᵢ}) τ]
        @∀r[(@repeated{βᵢ}) σ]]}
 @${@textsc{S-Poly-}α@textsc{-Equiv}}]

#:S-PolyD-α-Equiv

@$inferrule[
 @${@<:[@${τ[@repeated{αᵢ ↦ βᵢ} α ↦ β]} σ]}
 @${@<:[@∀r[(@repeated{αᵢ} @polydotα[α]) τ]
        @∀r[(@repeated{βᵢ} @polydotα[β]) σ]]}
 @${@textsc{S-PolyD-}α@textsc{-Equiv}}]

#:S-DFun-Fun*

@$inferrule[
 @${
  @repeated{@<:[σ_a τ_a]} \\
  @<:[σ @${τ[α ↦ ⊤]}] \\
  @<:R[R @${@R'}]}
 @${@<:[@∀r[(@polydotα[α]) @f…[(@repeated{τ_a} @polydot[τ α]) R]]
        @f*[(@repeated{σ_a} σ*) @${@R'}]]}
 @${@textsc{S-DFun-Fun*}}]

#:S-UnionSuper

@$inferrule[
 @${∃ i . @<:[τ @${σᵢ}]}
 @${@<:[τ @${⋃ @repeated{σᵢ}}]}
 @${@textsc{S-UnionSuper}}]

#:S-UnionSub

@$inferrule[
 @${@repeated[@<:[τᵢ @${σ}]]}
 @${@<:[@${⋃ @repeated{τᵢ}} σ]}
 @${@textsc{S-UnionSub}}]

#:S-Pair

@$inferrule[
 @${@<:[τ₁ σ₁] \\
  @<:[τ₂ σ₂]}
 @${@<:[@consτ[τ₁ τ₂] @consτ[σ₁ σ₂]]}
 @${@textsc{S-Pair}}]

#:S-DList

@$inferrule[
 @${@<:[τ σ]}
 @${@<:[@List…τ[τ α] @recτ[r @un[@consτ[σ r] @null-τ]]]}
 @${@textsc{S-DList}}]

#:S-IntersectionSub

@$inferrule[
 @${∃ i . @<:[@${σᵢ} τ]}
 @${@<:[@${⋂ @repeated{σᵢ}} τ]}
 @${@textsc{S-IntersectionSub}}]

#:S-IntersectionSuper

@$inferrule[
 @${@repeated[@<:[@${σ} τᵢ]]}
 @${@<:[σ @${⋂ @repeated{τᵢ}}]}
 @${@textsc{S-IntersectionSuper}}]

#:S-Promise

@$inferrule[
 @${@<:[τ σ]}
 @${@<:[@promiseτ[τ] @promiseτ[σ]]}
 @${@textsc{S-Promise}}]