#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Subtyping (with ρ)}

@$inferrule[@${∃ i . @<:[τ @${σᵢ}]}
            @${@<:[τ @variantτ[@repeated{σᵢ}]]}
            @${@textsc{S-VariantSuper}}]

@$inferrule[@${@repeated[@<:[τᵢ @${σ}]]}
            @${@<:[@variantτ[@repeated{τᵢ}] σ]}
            @${@textsc{S-VariantSub}}]


@;{@$inferrule[-
               @<:[@ctorτ[@κof[τ]] @ctorTop[τ]]
               @${@textsc{S-CtorTop}}]}

@$inferrule[
 @${@<:[τ τ′]}
 @<:[@ctorτ[@κof[τ]] @ctorτ[@κof[τ′]]]
 @${@textsc{S-Ctor}}]

@$inferrule[
 @${@repeated{@<:[τᵢ τ′ᵢ]}}
 @<:[@recordτ[@repeated{@${@|ɐ|ᵢ : τᵢ}}] @recordτ[@repeated{@${@|ɐ|ᵢ : τ'ᵢ}}]]
 @${@textsc{S-Record}}]

@$inferrule[
 @${@repeated{@<:[τⱼ τ′ⱼ]}}
 @<:[@recordτ[@repeated{@${@|ɐ|ⱼ : τⱼ}}]
     @recordτ[@|ρf| @repeatset{-@|ɐ|ᵢ} @repeatset{+@|ɐ|ⱼ:τ'ⱼ}]]
 @${@textsc{S-RecordF}}]

@$inferrule[
 @${@repeated{@<:[τₖ τ′ₖ]}}
 @<:[@recordτ[@|ρf| @repeatset{-@|ɐ|ᵢ} @repeatset{-@|ɐ|ⱼ} @repeatset{+@|ɐ|ₖ:τₖ}]
     @recordτ[@|ρf| @repeatset{-@|ɐ|ᵢ} @repeatset{+@|ɐ|ₖ:τ'ₖ}]]
 @${@textsc{S-FRecordF}}]

@$inferrule[@${@<:[@${τ[@repeated{@|ρf|ᵢ ↦ @|ρf|′ᵢ}]} σ]}
            @${@<:[@∀f[(@repeated{@|ρf|ᵢ}) τ]
                   @∀f[(@repeated{@|ρf|′ᵢ}) σ]]}
            @${@textsc{S-PolyF-}α@textsc{-Equiv}}]

@$inferrule[@${@<:[@${τ[@repeated{@|ρc|ᵢ ↦ @|ρc|′ᵢ}]} σ]}
            @${@<:[@∀c[(@repeated{@|ρc|ᵢ}) τ]
                   @∀c[(@repeated{@|ρc|′ᵢ}) σ]]}
            @${@textsc{S-PolyC-}α@textsc{-Equiv}}]

@$inferrule[-
            @=:[@variantτ[] ⊥]
            @${@textsc{S-Variant-Empty}}]

@; TODO: can these rules be used to combine a record type and a record*?
@; predicate to make sure some fields are absent, without checking anything
@; for the other fields?

@; TODO: instantiation rules. ∀ rules. Etc.

@;{
 Permutation of the fields of a record type produces an equivalent type:

 @$${
  @$inferrule[
 @${\phantom{x}}
 @<:[@recordτ[@repeated{@${@|ɐ|ᵢ : τᵢ}} @repeated{@${@|ɐ|ⱼ : τⱼ}}]
     @recordτ[@repeated{@${@|ɐ|ⱼ : τⱼ}} @repeated{@${@|ɐ|ᵢ : τᵢ}}]]
 @${@textsc{S-Record-Permuation}}
 ]
 }
}

@todo{propagate our types up/down unions like the primitive ones (I think
 Typed Racket does not do this everywhere it “should”).}