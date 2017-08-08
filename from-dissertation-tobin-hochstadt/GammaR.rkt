#lang at-exp s-exp phc-thesis/from-dissertation-tobin-hochstadt/lang-util

@; This file is not under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

#:Γ

@$${
 @Γ[⊢ e R]
}

#:R

@cases[@R #:first-sep "⩴"
       @R[τ
          @${φ⁺}
          @${φ⁻}
          @${o}]]