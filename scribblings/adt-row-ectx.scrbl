#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Evaluation contexts (with ρ)}

@todo{Does this need any change when adding row typing?}

@$${
 @cases["E" #:first-sep "⩴"
        @acase{…}
        @acase{@ctor[@κ E]}
        @acase{(@ctor-pred[@κ]\ E)}
        @acase{(@ctor-val[@κ]\ E)}
        @acase{@record[@repeated{@|ɐ|ᵢ = vᵢ}
                       @${@|ɐ|ⱼ = E}
                       @repeated{@|ɐ|ₖ = eₖ}]}
        @acase{(@record-pred[@repeated{@|ɐ|ᵢ}]\ E)}
        @acase{(@record-pred*[@repeated{@|ɐ|ᵢ} @repeated{-@|ɐ|ⱼ}]\ E)}
        @acase{E.@|ɐ|}
        @acase{@opwith[E @|ɐ| e]}
        @acase{@opwith[v @|ɐ| E]}
        @acase{@opwithout[E @|ɐ|]}]
}
