#lang at-exp s-exp phc-thesis/from-dissertation-tobin-hochstadt/lang-util

@; This file is NOT under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

@cases["E" #:first-sep "⩴"
       @acase{[] @tag*{program entry point}}
       ;@acase{@app[E @repeated{e}]}; I think this case is not needed.
       @acase{@app[@repeated{v} E @repeated{e}]@tag*{function application}}
       @acase{@ifop[E e e]@tag*{conditional}}
       @acase{@eq?op[E e]@tag*{symbol equality}}
       @acase{@eq?op[v E]}]