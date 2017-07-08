#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Subtyping (with ρ)}

@;{
 @$${
  @$inferrule[
 @${\phantom{x}}
 @${⊢ @ctor[@κ τ] <: @ctorTop[τ]}
 @${@textsc{S-CtorTop}}
 ]
 }
}

@$${
 @$inferrule[
 @${⊢ τ <: τ'}
 @${⊢ @ctor[@κ τ] <: @ctor[@κ @${τ'}]}
 @${@textsc{S-Ctor}}
 ]
}

@$${
 @$inferrule[
 @${@repeated{⊢ τᵢ <: τ'ᵢ}}
 @${⊢ @record[@repeated{@${@|ɐ|ᵢ : τᵢ}}] <: @record[@repeated{@${@|ɐ|ᵢ : τ'ᵢ}}]}
 @${@textsc{S-Record}}
 ]
}

Permutation of the fields of a record type produces an equivalent type:

@$${
 @$inferrule[
 @${\phantom{x}}
 @${⊢ @record[@repeated{@${@|ɐ|ᵢ : τᵢ}} @repeated{@${@|ɐ|ⱼ : τⱼ}}]
   <: @record[@repeated{@${@|ɐ|ⱼ : τⱼ}} @repeated{@${@|ɐ|ᵢ : τᵢ}}]}
 @${@textsc{S-Record-Permuation}}
 ]
}

@todo{propagate our types up/down unions like the primitive ones (I think
 Typed Racket does not do this everywhere it “should”).}