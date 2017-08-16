#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Evaluation contexts (with ρ)}

@$${
 @cases["E" #:first-sep "⩴"
        @acase{…}
        @acase{@ctor[@κ E]@tag*{constructor instance}}
        @acase{(@ctor-pred[@κ]\ E)@tag*{constructor predicate}}
        @acase{(@ctor-val[@κ]\ E)@tag*{constructor value access}}
        @acase{@record[@repeated{@|ɐ|ᵢ = vᵢ}
                       @${@|ɐ|ⱼ = E}
                       @repeated{@|ɐ|ₖ = eₖ}]@tag*{record instance}}
        @acase{(@record-pred[@repeatset{@|ɐ|ᵢ}]\ E)@tag*{record predicate}}
        @acase{(@record-pred*[@repeatset{@|ɐ|ᵢ} @repeatset{-@|ɐ|ⱼ}]\ E)@tag*{row-polymorphic record predicate}}
        @acase{E.@|ɐ|@tag*{record field access}}
        @acase{@opwith[E @|ɐ| e]@tag*{record update (new/change)}}
        @acase{@opwith[v @|ɐ| E]}
        @acase{@opwithout[E @|ɐ|]@tag*{record update (remove)}}]
}
