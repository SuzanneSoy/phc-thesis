#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Type validity rules (with ρ)}

@$inferrule[@${Δ ∪ \{ @repeated{@ρc} \} ⊢ τ}
            @${Δ ⊢ @∀c[(@repeated{@ρc}) τ]}
            @${@textsc{TE-CAll}}]

@$inferrule[@${Δ ∪ \{ @repeated{@ρf} \} ⊢ τ}
            @${Δ ⊢ @∀f[(@repeated{@ρf}) τ]}
            @${@textsc{TE-FAll}}]


@$inferrule[@${@ρc ∈ Δ \\
             \{@repeated{@|κ|ᵢ}\} ∩ \{@repeated{@|κ|ⱼ}\} = ∅ \\
             @alldifferent(@repeated{@|κ|ᵢ}) \\
             @alldifferent(@repeated{@|κ|ⱼ})}
            @${Δ ⊢ @variantτ[@ρc @repeatset{-@|κ|ᵢ} @repeatset{+@κof[ⱼ τⱼ]}]}
            @${@textsc{TE-CVariant}}]

where

@$${
 \begin{aligned}
 @alldifferent(@repeated{y}) &= @metatrue @textif ∀ i ≠ j . yᵢ ≠ yⱼ \\
 @alldifferent(@repeated{y}) &= @metafalse @otherwise
 \end{aligned}
}

@$inferrule[@${@ρf ∈ Δ \\
             \{@repeated{@|ɐ|ᵢ}\} ∩ \{@repeated{@|ɐ|ⱼ}\} = ∅ \\
             @alldifferent(@repeated{@|ɐ|ᵢ}) \\
             @alldifferent(@repeated{@|ɐ|ⱼ}) \\
             @repeated{Δ ⊢ τⱼ}}
            @${Δ ⊢ @recordτ[@ρf @repeatset{-@|ɐ|ᵢ} @repeatset{+@|ɐ|ⱼ:τⱼ}]}
            @${@textsc{TE-FRecord}}]

@$inferrule[@${@alldifferent(@repeated{@|κ|ᵢ}) \\
             @repeated{Δ ⊢ τᵢ}}
            @${Δ ⊢ @variantτ[@repeated{@ctorτ[@κof[ᵢ τᵢ]]}]}
            @${@textsc{TE-Variant}}]

@$inferrule[@${@alldifferent(@repeated{@|ɐ|ᵢ}) \\
             @repeated{Δ ⊢ τᵢ}}
            @${Δ ⊢ @recordτ[@repeated{@|ɐ|ᵢ : τᵢ}]}
            @${@textsc{TE-Record}}]

@$inferrule[@${@repeated{Δ ⊢ τ}}
            @${Δ ⊢ @ctorτ[@κof[τ]]}
            @${@textsc{TE-Ctor}}]