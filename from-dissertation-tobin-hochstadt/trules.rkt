#lang at-exp s-exp phc-thesis/from-dissertation-tobin-hochstadt/lang-util

@; This file is not under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

@(define _op @${_{\mathit{op}}})

#:

#:T-Promise

@$inferrule[@${@Γ[⊢ e @R[τ φ⁺ φ⁻ o]]}
            @${@Γ[⊢ @promisee[e] @R[@promiseτ[@R[τ φ⁺ φ⁻ o]] ϵ ⊥ ∅]]}
            @${@textsc{T-Promise}}]

#:T-Symbol

@$inferrule[-
            @${@Γ[⊢ @syme[s] @R[@symτ[s] ϵ ⊥ ∅]]}
            @${@textsc{T-Symbol}}]

#:T-Gensym

@$inferrule[-
            @${@Γ[⊢ @gensyme[] @R[@Symbolτ ϵ ⊥ ∅]]}
            @${@textsc{T-Gensym}}]

#:T-Eq?

@$inferrule[@${@Γ[⊢ e₁ @R[τ₁ φ⁺₁ φ⁻₁ o₂]] \\
             @Γ[⊢ e₂ @R[τ₂ φ⁺₂ φ⁻₂ o₂]] \\
             @<:[τ₁ @Symbolτ] \\
             @<:[τ₂ @Symbolτ]}
            @${@Γ[⊢ @eq?op[e₁ e₂] @R[@Booleanτ ϵ ⊥ ∅]]}
            @${@textsc{T-Eq}\mathrm{?}}]

#:T-Var

@$inferrule[-
            @${@Γ[⊢ x @R[@${Γ(x)} @${@!{@false-τ}} @true-τ x]]}
            @${@textsc{T-Var}}]

#:T-Primop

@$inferrule[-
            @${@Γ[⊢ p @R[@${δ_τ(p)} ϵ ⊥ ∅]]}
            @${@textsc{T-Primop}}]

#:T-True

@$inferrule[-
            @${@Γ[⊢ @true-e @R[@true-τ ϵ ⊥ ∅]]}
            @${@textsc{T-True}}]

#:T-False

@$inferrule[-
            @${@Γ[⊢ @false-e @R[@false-τ ⊥ ϵ ∅]]}
            @${@textsc{T-False}}]

#:T-Num

@$inferrule[-
            @${@Γ[⊢ @num-e @R[@num-τ ϵ ⊥ ∅]]}
            @${@textsc{T-Num}}]

#:T-Null

@$inferrule[-
            @${@Γ[⊢ @null-e @R[@null-τ ⊥ ϵ ∅]]}
            @${@textsc{T-Null}}]

#:T-DMap

@$inferrule[@${@Γ[⊢ e_r @R[@polydot[τ_r α] φ⁺_r φ⁻_r o_r]] \\
             @Γ[⊢ e_f @R[@∀r[(β) @f→[(@${τ_r@subst[α ↦ β]}) @R[τ φ⁺ φ⁻ o]]]
                         φ⁺_f
                         φ⁻_f
                         o_f]]}
            @${@Γ[⊢ @mapop[e_f e_r] @R[@polydot[@${τ@subst[β ↦ α]} α]
                                         ϵ
                                         ⊥
                                         ∅]]}
            @${@textsc{T-DMap}}]

#:T-AbsPred

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

#:T-Abs

@$inferrule[@${@Γ[⊢ e @R[τ φ⁺ φ⁻ o]]}
            @${@Γ[⊢ @λe[(@repeated{x:σ}) e]
                  @R[(f→ (@repeated{σ})
                         @R[τ
                            ϵ
                            ϵ
                            ∅])
                     ϵ ⊥ ∅]]}
            @${@textsc{T-Abs}}]

#:T-DAbs

@$inferrule[@${@repeated{Δ ⊢ τₖ} \\
             Δ ▷ @polydot[τ_r α] \\
             @Γ[@repeated{xₖ : τₖ} @${x_r : @polydot[τ_r α]} ⊢ e @R[τ φ⁺ φ⁻ o]]}
            @${@Γ[⊢ @λe[(@repeated{xₖ:τₖ} @${x_r:@polydot[τ_r α]}) e]
                  @R[(f… (@repeated{τₖ} @polydot[τ_r α])
                         @R[τ
                            ϵ
                            ϵ
                            ∅])
                     ϵ ⊥ ∅]]}
            @${@textsc{T-DAbs}}]

#:T-TAbs

@$inferrule[@${@Γ[Δ @${\{@repeatset{α}\}} ⊢ e @R[τ φ⁺ φ⁻ o]]}
            @${@Γ[⊢ @Λe[(@repeated{α}) e]
                  @R[(∀r (@repeated{α})
                         @R[τ
                            ϵ
                            ϵ
                            ∅])
                     ϵ ⊥ ∅]]}
            @${@textsc{T-TAbs}}]

#:T-DTAbs

@$inferrule[@${@Γ[Δ @${\{@repeatset{α}\}} @${\{@polydotα{β}\}}
                  ⊢ e @R[τ φ⁺ φ⁻ o]]}
            @${@Γ[⊢ @Λe[(@repeated{α} @polydotα{β}) e]
                  @R[(∀r (@repeated{α} @polydotα{β})
                         @R[τ
                            ϵ
                            ϵ
                            ∅])
                     ϵ ⊥ ∅]]}
            @${@textsc{T-DTAbs}}]

#:substφ

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

#:substo

@$${
 \begin{aligned}
 π(x)@substφo[x ↦ ∅] &= ∅ &\\
 π(x)@substφo[x ↦ z] &= π(z) &@textif z ≠ ∅ \\
 π(y)@substφo[x ↦ z] &= ∅ &@textif y ≠ x \\
 ∅@substφo[x ↦ z] &= ∅ &
 \end{aligned}
}

#:T-App

@$inferrule[@${@Γ[⊢ @${e@_op} @R[@${τ@_op} @${φ⁺@_op} @${φ⁻@_op} @${o@_op}]] \\
             @repeated[@Γ[⊢ @${aᵢ}
                          @R[@${τ_{aᵢ}} @${φ⁺_{aᵢ}} @${φ⁻_{aᵢ}} @${o_{aᵢ}}]]] \\
             @repeated[@<:[τ_a @${τ_{\mathit{in}}}]]
             @<:[@${τ@_op} @f→[(@repeated{τ_{\mathit{in}}})
                               @R[τ_r φ⁺_r φ⁻_r o_r]]]
             φ⁺_r' = φ⁺_r@substφo[• ↦ @${o_{a_0}}] \\
             φ⁻_r' = φ⁻_r@substφo[• ↦ @${o_{a_0}}] \\
             o' = o@substφo[• ↦ @${o_{a_0}}]}
            @${@Γ[⊢ @app[@${e@_op} @repeated{aᵢ}]
                  @R[(f→ (@repeated{σ})
                         @R[τ_r
                            @${φ⁺'}
                            @${φ⁻'}
                            @${o'}])
                     ϵ ⊥ ∅]]}
            @${@textsc{T-App}}]

#:T-Inst

@$inferrule[@${@repeated{Δ ⊢ τⱼ} \\
             @Γ[⊢ @${e@_op} @R[@∀r[(@repeated{αⱼ}) τ] φ⁺ φ⁻ o]]}
            @Γ[⊢ @at[@${e@_op} @repeated{τⱼ}]
               @R[@${τ@subst[@repeated{aⱼ ↦ τⱼ}]}
                  ϵ ϵ ∅]]
            @${@textsc{T-Inst}}]

#:T-DInst

@$inferrule[@${@repeated[#:n "n"]{Δ ⊢ τⱼ} \\
             @repeated[#:n "m"]{Δ ⊢ τₖ} \\
             @Γ[⊢ @${e@_op} @R[@∀r[(@repeated[#:n "n"]{αⱼ} @polydotα[β]) τ]
                                 φ⁺ φ⁻ o]]}
            @Γ[⊢ @at[@${e@_op} @repeated[#:n "n"]{τⱼ} @repeated[#:n "m"]{τₖ}]
               @R[@transdots[@${τ@subst[@repeated[#:n "n"]{aⱼ ↦ τⱼ}]}
                             @${β}
                             @repeated[#:n "m"]{τₖ}]
                  ϵ ϵ ∅]]
            @${@textsc{T-DInst}}]

#:T-DInstD

@$inferrule[@${@repeated{Δ ⊢ τₖ} \\
             Δ ▷ @polydot[τ_r β] \\
             @Γ[⊢ @${e@_op} @R[@∀r[(@repeated{αₖ} @polydotα[α_r]) τ]
                                 φ⁺ φ⁻ o]]}
            @Γ[⊢ @at[@${e@_op} @repeated{τₖ} @polydot[τ_r β]]
               @R[@substdots[@${τ@subst[@repeated{aₖ ↦ τₖ}]}
                             @${α_r}
                             @${τ_r}
                             @${β}]
                  ϵ ϵ ∅]]
            @${@textsc{T-DInstD}}]

#:T-If

@$inferrule[@${@Γ[⊢ @${e₁} @R[@${τ₁} @${φ⁺₁} @${φ⁻₁} @${o₁}]] \\
             @Γ[+ φ⁺₁ ⊢ @${e₂} @R[@${τ₂} @${φ⁺₂} @${φ⁻₂} @${o₂}]] \\
             @Γ[+ φ⁻₁ ⊢ @${e₃} @R[@${τ₃} @${φ⁺₃} @${φ⁻₃} @${o₃}]] \\
             @<:[τ₂ τ_r] \\
             @<:[τ₃ τ_r] \\
             φ_r⁺ / φ_r⁻ = @combinefilter(φ⁺₁ / φ⁻₁, φ⁺₂ / φ⁻₂, φ⁺₃ / φ⁻₃) \\
             o_r = \begin{cases}
             o₂ @& @textif o₂ = o₃
             @nl ∅ @& @otherwise
             \end{cases}}
            @${@Γ[⊢ @ifop[e₁ e₂ e₃] @R[τ_r φ_r⁺ φ_r⁻ o_r]]}
            @${@textsc{T-If}}]

#:Γ+

@aligned{
 Γ + \{τ_{π(x)}\} ∪ @repeatset{ψ}
 &= (Γ, x : @update(Γ(x), τ_π)) + @repeatset{ψ}\\
 Γ + \{@!{τ}_{π(x)}\} ∪ @repeatset{ψ}
 &= (Γ, x : @update(Γ(x), @!{τ}_π)) + @repeatset{ψ}\\
 Γ + \{⊥\} ∪ @repeatset{ψ} &= Γ'
 @where ∀x∈ \operatorname{dom}(Γ).@=:[@${Γ'(x)} ⊥]\\
 Γ + ϵ &= Γ \\
}

#:update

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

#:restrict

@aligned{
 @restrict(τ, σ) &= ⊥ &@textif @no-overlap(τ,σ)\\
 @restrict((⋃ @repeatset{τ}), σ) &= (⋃ @repeatset{@restrict(τ,σ)}) &\\
 @restrict(τ, σ) &= τ &@textif @<:[τ σ]\\
 @restrict(τ, σ) &= σ &@otherwise
}

#:remove

@aligned{
 @remove(τ, σ) &= ⊥ &@textif @<:[τ σ] \\
 @remove((⋃ @repeatset{τ}), σ) &= (⋃ @repeatset{@remove(τ,σ)} &\\
 @remove(τ, σ) &= τ &@otherwise
}

#:no-overlap

@aligned{
 @no-overlap(τ, τ′) &= @metatrue
 &&@textif ∄ σ .
 \quad @<:[σ τ]\quad
 ∧ \quad@<:[σ τ′]\quad
 ∧ \quadΔ ⊢ σ\quad
 ∧ \quad@≠:[σ ⊥]\\
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

#:combinefilter

@aligned{
 @combinefilter(ϵ / ⊥, φ^±₂, φ^±₃) &= φ^±₂ \\
 @combinefilter(⊥ / ϵ, φ^±₂, φ^±₃) &= φ^±₃ \\
 @combinefilter(⊥ / ⊥, φ^±₂, φ^±₃) &= ⊥ &\\
 @combinefilter(φ⁺₁ / φ⁻₁, φ⁺₂ / φ⁻₂, ⊥/ϵ) &= φ⁺₁ ∪ φ⁺₂ \\
 @combinefilter(
 \{ τ_{π(@loc)} \} ∪ φ⁺₁ / \{ @!{τ}_{π(@loc)} \} φ⁻₁,
 ϵ / ⊥,
 ⊥/ϵ)
 &= (∪\ τ\ σ)_{π(@loc)} / @!{(∪\ τ\ σ)_{π(@loc)}} \\
 … & = … & \\
 @combinefilter(φ^±₁, φ^±₂, φ^±₃) &= ϵ / ϵ \qquad @otherwise \\
}
