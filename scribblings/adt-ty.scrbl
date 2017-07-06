#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Types}

@$${σ,τ ⩴ …
 @P @ctor[@κ τ] @;{@P @ctorTop[τ]}
 @P @record[@repeated{@|ɐ|ᵢ : τᵢ}]}

