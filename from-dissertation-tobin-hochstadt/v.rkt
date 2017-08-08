#lang at-exp s-exp phc-thesis/from-dissertation-tobin-hochstadt/lang-util

@; This file is not under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

@cases["v" #:first-sep "⩴"
       @acase{@primop @tag*{primitive function}}
       @acase{@num-v @tag*{number}}
       @acase{@true-v @tag*{booleans}}
       @acase{@false-v}
       @acase{@λv[ℰ (@repeated{x:τ}) e] @tag*{lambda function}}
       @acase{@λv[ℰ (@repeated{x:τ} @${\ .\ } @${x:τ*}) e]
        @tag*{variadic function}}
       @acase{@λv[ℰ (@repeated{x:τ} @${\ .\ } @${x:@polydot[τ α]}) e]
        @tag*{variadic polymorphic function}}
       @acase{@Λv[ℰ (@repeated{α}) e]
        @tag*{polymorphic abstraction}}
       @acase{@Λv[ℰ (@repeated{α} @polydotα[α]) e]
        @tag*{variadic polymorphic abstraction}}
       @acase{@consv[v v] @tag*{pair}}
       @acase{@null-v @tag*{null}}
       @acase{@promisev[ℰ e] @tag*{promise}}
       @acase{@symv[@sym*] @tag*{symbol}}]