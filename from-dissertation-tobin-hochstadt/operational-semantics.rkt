#lang at-exp s-exp phc-thesis/from-dissertation-tobin-hochstadt/lang-util

@; This file is NOT under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

#:E-Delta

@$inferrule[@${δ(c,v) = v'}
            @${(c v) ↪ v'}
            @${@textsc{E-Delta}}]

#:E-Beta

@$inferrule[-
            @${@app[@λe[(@repeated{x:τ}) e_b] @repeated{e_a}]
             ↪ e_b@subst[@repeated{x ↦ e_a}]}
            @${@textsc{E-Beta}}]

#:E-Beta*

@$inferrule[-
            @${
             @app[@λe[(@repeated[#:n "n"]{x:τ} @${\ .\ } @${x_r:τ_r*}) e_b]
                  @repeated[#:n "n"]{e_a} @repeated[#:n "m"]{e_r}]
             ↪ e_b@subst[x_r ↦ @listv[@repeated[#:n "m"]{e_r}]
                         @repeated[#:n "n"]{x ↦ e_a}]}
            @${@textsc{E-Beta*}}]

#:E-TBeta

@$inferrule[-
            @${@at[@Λe[(@repeated{α}) e] @repeated{τ}] ↪ e}
            @${@textsc{E-TBeta}}]

#:E-TDBeta

@$inferrule[-
            @${@at[@Λe[(@repeated{α} @polydotα[β]) e] @repeated{τ}] ↪ e}
            @${@textsc{E-TDBeta}}]

@λe[(@repeated{x:τ} @${\ .\ } @${x:τ*}) e]

#:E-IfFalse

@$inferrule[-
            @${@ifop[@false-v e₂ e₃] ↪ e₃}
            @${@textsc{E-IfFalse}}]

#:E-IfTrue

@$inferrule[-
            @${@ifop[@false-v e₂ e₃] ↪ e₃}
            @${@textsc{E-IfTrue}}]

#:E-Context

;; TODO: check what this means (and is it the right kind of arrow?)
@$inferrule[@${L ↪ R}
            @${E[L] → E[R]}
            @${@textsc{E-Context}}]