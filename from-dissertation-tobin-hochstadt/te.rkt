#lang at-exp s-exp phc-thesis/from-dissertation-tobin-hochstadt/lang-util

@; This file is not under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

#:TE-Var

@$inferrule[@${α ∈ Δ}
            @${Δ ⊢ α}
            @${@textsc{TE-Var}}]

#:TE-DList

@$inferrule[@${@polydotα[α] ∈ Δ \\ Δ ∪ \{α\} ⊢ τ}
            @${Δ ⊢ @List…τ[τ α]}
            @${@textsc{TE-DList}}]

#:TE-All

@$inferrule[@${Δ ∪ \{@repeated{αᵢ}\} ⊢ τ}
            @${Δ ⊢ @∀r[(@repeated{αᵢ}) τ]}
            @${@textsc{TE-All}}]

#:TE-DFun

@$inferrule[@${Δ ▷ @polydot[τ_r α] \\ @repeated{Δ ⊢ τᵢ} \\ Δ ⊢ τ}
            @${Δ ⊢ @f…[(@repeated{τᵢ} @polydot[τ_r α]) @R[τ φ⁺ φ⁻ o]]}
            @${@textsc{TE-DFun}}]

#:TE-DAll

@$inferrule[@${Δ ∪ \{@repeated{αᵢ}\ @polydotα[β]\} ⊢ τ}
            @${Δ ⊢ @∀r[(@repeated{αᵢ} @polydotα[β]) τ]}
            @${@textsc{TE-DAll}}]

#:TE-DPretype

@$inferrule[@${@polydotα[α] ∈ Δ \\ Δ ∪ \{α\} ⊢ τ}
            @${Δ ▷ @polydot[τ α]}
            @${@textsc{TE-DPretype}}]

#:TE-Rec

@$inferrule[@${Δ ∪ \{r\} ⊢ τ}
            @${Δ ⊢ @recτ[r τ]}
            @${@textsc{TE-Rec}}]

#:TE-Trivial

@$p[
 @$inferrule[-
             @${Δ ⊢ ⊤}
             @${@textsc{TE-Top}}]

 @$inferrule[-
             @${Δ ⊢ @num-τ[n]}
             @${@textsc{TE-Num}}]
       
 @$inferrule[-
             @${Δ ⊢ @Numberτ}
             @${@textsc{TE-Number}}]

 @$inferrule[-
             @${Δ ⊢ @true-τ}
             @${@textsc{TE-True}}]

 @$inferrule[-
             @${Δ ⊢ @false-τ}
             @${@textsc{TE-False}}]

 @$inferrule[-
             @${Δ ⊢ @symτ[s]}
             @${@textsc{TE-Sym}}]

 @$inferrule[-
             @${Δ ⊢ @Symbolτ}
             @${@textsc{TE-Symbol}}]

 @; function/poly × 5

 @$inferrule[@${@repeated{Δ ⊢ τᵢ}}
             @${Δ ⊢ @un[@repeatset{τᵢ}]}
             @${@textsc{TE-Union}}]

 @$inferrule[@${@repeated{Δ ⊢ τᵢ}}
             @${Δ ⊢ @∩τ[@repeatset{τᵢ}]}
             @${@textsc{TE-Intersection}}]

 @$inferrule[@${Δ ⊢ τ \\ Δ ⊢ σ}
             @${Δ ⊢ @consτ[τ σ]}
             @${@textsc{TE-Pair}}]

 @$inferrule[-
             @${Δ ⊢ @null-τ}
             @${@textsc{TE-Null}}]

 @$inferrule[@${Δ ⊢_R R}
             @${Δ ⊢ @promiseτ[R]}
             @${@textsc{TE-Promise}}]
 ]

#:TE-R

;; No particular restrictions on the object at this point (should we put some to
;; make sure that it exists?)
@$inferrule[@${Δ ⊢ τ \\ Δ ⊢_{\mathrm{φ}} φ⁺ \\ Δ ⊢_{\mathrm{φ}} φ⁻}
            @${Δ ⊢_R @R[τ φ⁺ φ⁻ o]}
            @${@textsc{TE-R}}]

#:TE-Phi

@$inferrule[@${@repeated{Δ ⊢_{\mathrm{ψ}} ψ}}
            @${Δ ⊢_{\mathrm{φ}} @repeatset{ψ}}
            @${@textsc{TE-Phi}}]

#:TE-Psi

;; TODO? should we impose that the @loc is within scope, and that the π is
;; valid? (same thing for the o in TE-R?)
@$inferrule[@${@repeated{Δ ⊢ τ}}
            @${Δ ⊢_{\mathrm{ψ}} τ_{π(@loc)}}
            @${@textsc{TE-Psi}}]

#:TE-Psi-Not

@$inferrule[@${@repeated{Δ ⊢ τ}}
            @${Δ ⊢_{\mathrm{ψ}} @!{τ}_{π(@loc)}}
            @${@textsc{TE-Psi-Not}}]

#:TE-Psi-Bot

@$inferrule[-
            @${Δ ⊢_{\mathrm{ψ}} ⊥}
            @${@textsc{TE-Psi-Bot}}]
