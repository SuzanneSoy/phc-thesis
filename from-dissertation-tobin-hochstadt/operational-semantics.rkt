#lang at-exp s-exp phc-thesis/scribblings/equations-lang

@; This file is NOT under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

#:E-Delta

@$inferrule[@${δ(@primop,@repeated{v}) = v'}
            @${@app[@primop @repeated{v}] ↪ v'}
            @${@textsc{E-Delta}}]

#:E-DeltaE

@$inferrule[@${@δe(e) = v}
            @${e ↪ v'}
            @${@textsc{E-DeltaE}}]

#:δ-rules

@$${
 \begin{aligned}
 δ(@textit{add1}, @num-v*{n}) &= @num-v*{n+1} & \\
 δ(@textit{number?}, @num-v) &= @true-v & \\
 δ(@textit{number?}, v) &= @false-v & @text{otherwise} \\
 δ(@consp, v₁, v₂) &= @consv[v₁ v₂] & \\
 δ(@textit{car}, @consv[v₁ v₂]) &= v₁ & \\
 δ(@textit{cdr}, @consv[v₁ v₂]) &= v₂ & \\
 δ(@textit{pair?}, @consv[v v]) &= @true-v & \\
 δ(@textit{pair?}, v) &= @false-v & @text{otherwise} \\
 δ(@textit{null?}, @null-v) &= @true-v & \\
 δ(@textit{null?}, v) &= @false-v & @text{otherwise} \\
 δ(@textit{identity}, v) &= v & \\
 \end{aligned}
}

#:δe-rules

@$${
 \begin{aligned}
 @δe(@num-e) = @num-v \\
 @δe(@true-e) = @true-v \\
 @δe(@false-e) = @false-v \\
 @δe(@null-e) = @null-v \\
 @δe(@λe[(@repeated{x:τ}) e]) = @λv[(@repeated{x_r:τ}) e] \\
 @δe(@λe[(@repeated{x:τ} @${\ .\ } @${x_r:σ*}) e])
 = @λv[(@repeated{x:τ} @${\ .\ } @${x:σ*}) e] \\
 @δe(@λe[(@repeated{x:τ} @${\ .\ } @${x_r:@polydot[σ α]}) e])
 = @λv[(@repeated{x:τ} @${\ .\ } @${x_r:@polydot[σ α]}) e] \\
 @δe(@Λe[(@repeated{α}) e]) = @Λv[(@repeated{α}) e] \\
 @δe(@Λe[(@repeated{α} @polydotα[β]) e]) = @Λv[(@repeated{α} @polydotα[β]) e] \\
 @δe(@promisee[e]) = @promisev[e] \\
 @δe(@syme[s]) = @symv[s]
 \end{aligned}
}

#:E-Beta

@$inferrule[-
            @${@app[@λv[(@repeated{x:τ}) e_b] @repeated{e_a}]
             ↪ e_b@subst[@repeated{x ↦ e_a}]}
            @${@textsc{E-Beta}}]

#:E-Beta*

@$inferrule[-
            @${
             @app[@λv[(@repeated[#:n "n"]{x:τ} @${\ .\ } @${x_r:τ_r*}) e_b]
                  @repeated[#:n "n"]{e_a} @repeated[#:n "m"]{e_r}]
             ↪ e_b@subst[x_r ↦ @listv[@repeated[#:n "m"]{e_r}]
                         @repeated[#:n "n"]{x ↦ e_a}]}
            @${@textsc{E-Beta*}}]

#:E-BetaD

@$inferrule[-
            @${
             @app[@λv[(@repeated[#:n "n"]{x:τ} @${\ .\ }
                        @${x_r:@polydot[τ_r α]}) e_b]
                  @repeated[#:n "n"]{e_a} @repeated[#:n "m"]{e_r}]
             ↪ e_b@subst[x_r ↦ @listv[@repeated[#:n "m"]{e_r}]
                         @repeated[#:n "n"]{x ↦ e_a}]}
            @${@textsc{E-BetaD}}]

#:E-TBeta

@$inferrule[-
            @${@at[@Λv[(@repeated{α}) e] @repeated{τ}] ↪ e}
            @${@textsc{E-TBeta}}]

#:E-TDBeta

@$inferrule[-
            @${@at[@Λv[(@repeated{α} @polydotα[β]) e] @repeated{τ}] ↪ e}
            @${@textsc{E-TDBeta}}]

@λe[(@repeated{x:τ} @${\ .\ } @${x:τ*}) e]

#:E-If-False

@$inferrule[-
            @${@ifop[@false-v e₂ e₃] ↪ e₃}
            @${@textsc{E-If-False}}]

#:E-If-True

@$inferrule[@${v ≠ @false-v}
            @${@ifop[v e₂ e₃] ↪ e₃}
            @${@textsc{E-If-True}}]

#:E-Force

@$inferrule[@${e ↪ v}
            @${@forcee[@promisev[e]] ↪ v}
            @${@textsc{E-Force}}]

#:E-Gensym

@$inferrule[@${v = @sym* @text{ fresh}}
            @${@gensyme[] ↪ v}
            @${@textsc{E-Gensym}}]

#:E-Eq?-True

@$inferrule[@${v₁ = v₂}
            @${@eq?op[v₁ v₂] ↪ @true-v}
            @${@textsc{E-Eq?-True}}]

#:E-Eq?-False

@$inferrule[@${v₁ ≠ v₂}
            @${@eq?op[v₁ v₂] ↪ @false-v}
            @${@textsc{E-Eq?-False}}]

#:E-Map-Pair

@$inferrule[@${}
            @${@mapop[v_f @consv[v₁ v₂]] ↪ @consp[@app[v_f v₁] @mapop[v_f v₂]]}
            @${@textsc{E-Map-Pair}}]

#:E-Map-Null

@$inferrule[@${}
            @${@mapop[v_f @null-v] ↪ @null-v}
            @${@textsc{E-Map-Null}}]

#:E-Context

;; TODO: check what this means (and is it the right kind of arrow?)
@$inferrule[@${L ↪ R}
            @${E[L] ↪ E[R]}
            @${@textsc{E-Context}}]
