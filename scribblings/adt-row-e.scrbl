#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Expressions}

@$${
 @cases["e" #:first-sep "⩴"
        @acase{…}
        @acase{@ctor[@κ e]}
        @acase{(@ctor-pred[@κ] e)}
        @acase{(@ctor-val[@κ] e)}
        @acase{@record[@repeated{@|ɐ|ᵢ = eᵢ}]}
        @acase{(@record-pred[@repeated{@|ɐ|ᵢ}] e)}
        @acase{e.@|ɐ|}
        @acase{@opwith[e @|ɐ| e]}
        @acase{@opwithout[e @|ɐ|]}]
}

@;{
 Note: In the @${@record[@repeated{@|ɐ|ᵢ = eᵢ}]} expression, which builds a
 new record value, the @${@|ɐ|ᵢ} are ordered, and the field order defines the
 order of evaluation, as indicated by the extensions to the @${E} contexts
 which is given below. In other uses, the order of fields within the record is
 irrelevant, i.e. the record can be assimilated to a set of 2-uples, of which
 the first element is a field name, and the second a value or type.
}
