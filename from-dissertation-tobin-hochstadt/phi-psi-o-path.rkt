#lang at-exp s-exp phc-thesis/from-dissertation-tobin-hochstadt/lang-util

@; This file is NOT under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

#:φ

@cases[@${φ} #:first-sep "⩴" @acase{@repeatset{ψ}@tag*{filter set}}]

#:ψ

@cases["ψ" #:first-sep "⩴"
       @acase{τ_{π(@loc)}
        @tag*{@${(ℰ[v] = \mathbf{?}) ⇒ ℰ[π(@loc)]@text{ is of type @${τ}}}}}
       @acase{@!{τ}_{π(@loc)}
        @tag*{@${(ℰ[v] = \mathbf{?}) ⇒ ℰ[π(@loc)]@text{ is not of type @${τ}}}}}
       @acase{⊥@tag*{contradiction}}]

#:loc

@cases[@loc #:first-sep "⩴"
       @acase{•@tag*{function's first argument}}
       @acase{x@tag*{variable}}]

#:o

@cases[@textrm{o} #:first-sep "⩴"
       @acase{π(@loc)@tag*{@${e} is an alias for @${π(@loc)}}}
       @acase{∅@tag*{no aliasing information}}]

#:π

@cases[@textit{π} #:first-sep "⩴"
       @acase{pe∷π@tag*{path concatenation}}
       @acase{@emptypath @tag*{empty path}}]

#:pe

@cases[@textit{pe} #:first-sep "⩴"
       @acase{@carπ @tag*{first element of pair}}
       @acase{@cdrπ @tag*{second element of pair}}
       @acase{@forceπ @tag*{result of a promise}}]