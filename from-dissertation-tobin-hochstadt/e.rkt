#lang at-exp s-exp phc-thesis/scribblings/equations-lang

@; This file is NOT under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

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
       @acase{@promisee[e] @tag*{create promise}}
       @acase{@forcee[e] @tag*{force promise}}@;TODO: shouldn't it be a primop?
       @acase{@syme[s] @tag*{symbol literal}}
       @acase{@gensyme[] @tag*{fresh uninterned symbol}}
       @acase{@eq?op[e e] @tag*{symbol equality}}
       @acase{@mapop[e e]}]

#:sym

@$${
 \begin{aligned}
 s & ∈ 𝒮\\
 @sym* & ∈ @𝒮* \\
 𝒮 & ⊂ @𝒮*
 \end{aligned}
}
