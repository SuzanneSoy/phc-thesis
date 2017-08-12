#lang at-exp s-exp phc-thesis/scribblings/equations-lang

@; This file is NOT under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

#:Simplify1

@$${
 \begin{aligned}
 @simplify[@∩τ[τ τ′ @repeatset{τ″}]]
 &= @simplify[@∩τ[@∩τ[τ τ′] @repeatset{τ″}]] \\
 @simplify[@∩τ[@un[@repeatset{τ}] @repeatset{σ}]]
 &= @propagate⊥(@un[@repeatset{@simplify[@∩τ[τ @repeatset{σ}]]}])\\
 @simplify[@∩τ[@consτ[τ τ′] @consτ[σ σ′]]]
 &= @(∩τ @${@propagate⊥(@(consτ @simplify[@∩τ[τ σ]]
                                @simplify[@∩τ[τ′ σ′]]))})\\
 @simplify[@∩τ[@repeatset{σ}]]
 &= \begin{cases}
 ⊥ & @textif ∃ τ, τ′ ∈ \{@repeatset{σ}\} . @no-overlap(τ, τ′) \\
 @∩τ[@repeatset{@simplify[σ]}] &@otherwise
 \end{cases}
 \end{aligned}
}

#:Simplify2

@$${
 \begin{aligned}
 @simplify[@f→[(@repeated{τ}) R]]
 &= @f→[(@repeated{@simplify[τ]}) @simplify[R]] \\
 @simplify[@f*[(@repeated{τ} @${τ*}) R]]
 &= @f*[(@repeated{@simplify[τ]} @simplify[τ*]) @simplify[R]]\\
 @simplify[@f…[(@repeated{τ} @polydot[τ α]) R]]
 &= @f…[(@repeated{@simplify[τ]} @polydot[@simplify[τ] α]) @simplify[R]]\\
 @simplify[@∀r[(@repeated{α}) τ]]
 &= @∀r[(@repeated{α}) @simplify[τ]]\\
 @simplify[@∀r[(@repeated{α} @polydotα[α]) τ]]
 &= @∀r[(@repeated{α} @polydotα[α]) @simplify[τ]]\\
 @simplify[@consτ[τ σ]]
 &= @consτ[@${@simplify[τ]} @${@simplify[σ]}]\\
 @simplify[@List…τ[τ α]]
 &= @List…τ[@simplify[τ] α]\\
 @simplify[@promiseτ[R]]
 &= @promiseτ[@simplify[R]]\\
 @simplify[@recτ[r τ]]
 &= @recτ[r @simplify[τ]] \\
 @simplify[τ] &= τ \qquad @otherwise \\
 @simplify[@R[τ φ⁺ φ⁻ o]] &= @R[@simplify[τ] @simplify[φ⁺] @simplify[φ⁻] o]\\
 @simplify[@repeatset{ψ}] &= @repeatset{@simplify[ψ]}\\
 @simplify[@${τ_{π(loc)}}] &= @simplify[τ]_{π(loc)}\\
 @simplify[@${@!{τ}_{π(loc)}}] &= @!{@simplify[τ]}_{π(loc)}\\
 @simplify[⊥] = ⊥
 \end{aligned}
}

#:Propagate⊥

@$${
 \begin{aligned}
 @propagate⊥(@consτ[τ σ]) &= ⊥ & @textif @=:[τ ⊥]\\
 @propagate⊥(@consτ[τ σ]) &= ⊥ & @textif @=:[σ ⊥]\\
 @propagate⊥(τ) &= ⊥ &@textif @=:[τ ⊥]\\@; For unions (subtyping will dispatch)
 @propagate⊥(τ) &= τ &@otherwise
 \end{aligned}
}
