#lang at-exp s-exp phc-thesis/from-dissertation-tobin-hochstadt/lang-util

@; This file is not under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

@$${
 \begin{aligned}
 δ(@textit{add1}) =
 @f→[(Numberτ) @R[Numberτ ϵ ⊥ ∅]] \\
 δ(@textit{number?}) =
 @f→[(⊤) @R[Boolean @${@|Numberτ|_•} @${@![Numberτ]_•} ∅]] \\
 δ(@textit{pair?}) =
 @f→[(⊤) @R[Boolean @${@consτ[⊤ ⊤]_•} @${@![@consτ[⊤ ⊤]]_•} ∅]] \\
 δ(@textit{null?}) =
 @f→[(⊤) @R[Boolean @${@|null-τ|_•} @${@![null-τ]_•} ∅]] \\
 δ(@textit{identity}) = @∀r[(α) @f→[(α) @R[α
                                           @${@![false-v]_{•}}
                                           @${@|false-v|_{•}}
                                           @${•}]]] \\
 δ(@consp) = @∀r[(α β) @f→[(α β) @R[@consτ[α β] ϵ ⊥ ∅]]] \\
 δ(@textit{car}) = @∀r[(α β) @f→[(@consτ[α β]) @R[α
                                                  @${@![false-v]_{car(•)}}
                                                  @${@|false-v|_{car(•)}}
                                                  @${car(•)}]]] \\
 δ(@textit{cdr}) = @∀r[(α β) @f→[(@consτ[α β]) @R[β
                                                  @${@![false-v]_{cdr(•)}}
                                                  @${@|false-v|_{cdr(•)}}
                                                  @${cdr(•)}]]] \\
 \end{aligned}
}