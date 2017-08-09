#lang at-exp s-exp phc-thesis/from-dissertation-tobin-hochstadt/lang-util

@; This file is NOT under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

@cases["ℰ" #:first-sep "⩴"
       @acase{@repeated{@↦v[x v]}
        \ @repeated{@↦v[α τ]}
        \ @repeated{@↦v[@polydotα[α] @repeated{τ}]}
        @tag*{bound variables @${\&} types}}]