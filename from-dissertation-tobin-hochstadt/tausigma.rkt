#lang at-exp s-exp phc-thesis/scribblings/equations-lang

@; This file is NOT under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

@cases["τ,σ" #:first-sep "⩴"
       @acase{⊤@tag*{top}}
       @acase{@num-τ @tag*{number singleton}}
       @acase{@Numberτ @tag*{any number}}
       @acase{@true-τ @tag*{boolean singleton}}
       @acase{@false-τ}
       @acase{@symτ[@sym*] @tag*{symbol singleton}}
       @acase{@Symbolτ @tag*{any symbol}}
       @acase{@f→[(@repeated{τ}) R] @tag*{function}}
       @acase{@f*[(@repeated{τ} @${τ*}) R] @tag*{variadic function}}
       @acase{@f…[(@repeated{τ} @polydot[τ α]) R]
        @tag*{variadic polymorphic function}}
       @acase{@∀r[(@repeated{α}) τ]@tag*{polymorphic type}}
       @acase{@∀r[(@repeated{α} @polydotα[α]) τ]
        @tag*{variadic polymorphic type}}
       @acase{α @P β@tag*{polymorphic type variable}}
       @acase{@un[@repeatset{τ}]@tag*{union}}
       @acase{@∩τ[@repeatset{τ}] @tag*{intersection}}
       @acase{@consτ[τ τ]@tag*{pair}}
       @acase{@null-τ @tag*{null (end of lists)}}
       @acase{@List…τ[τ α] @tag*{variadic polymorphic list}}
       @acase{@promiseτ[R] @tag*{promise}}
       @acase{@recτ[r τ] @tag*{recursive type}}]

#:Boolean

@$${
 @=:def[@Booleanτ @un[@true-τ @false-τ]]
}

#:Listτ

@$${
 \begin{aligned}
 @=:def[@Listτ[τ @repeated{σ}] @consτ[τ @Listτ[@repeated{σ}]]] \\
 @=:def[@Listτ[] @null-τ]
 \end{aligned}
}

#:Listofτ

@$${
 @=:def[@Listofτ[τ] @recτ[r @un[@null-τ @consτ[τ r]]]]
}