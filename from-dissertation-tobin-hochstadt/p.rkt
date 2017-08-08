#lang at-exp s-exp phc-thesis/from-dissertation-tobin-hochstadt/lang-util

@; This file is not under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

@cases[@primop #:first-sep "⩴"
       @acase{@textit{add1}@tag*{returns its argument plus @${1}}}
       @acase{@textit{number?}@tag*{number predicate}}
       @acase{@textit{pair?}@tag*{pair predicate}}
       @acase{@textit{null?}@tag*{@null-v predicate}}
       @acase{@textit{identity}@tag*{identity function}}
       @acase{@consp @tag*{pair construction}}
       @acase{@textit{car}@tag*{first element of pair}}
       @acase{@textit{cdr}@tag*{second element of pair}}
       @acase{…}]