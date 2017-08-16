#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Type validity rules (with ρ)}

@$${
 @$inferrule[
 @${Δ ∪ \{ @repeated{@ρc} \} ⊢ τ}
 @${Δ ⊢ (∀_c (@repeated{@ρc}) τ)}
 @${@textsc{TE-CAll}}
 ]
}

@$${
 @$inferrule[
 @${Δ ∪ \{ @repeated{@ρf} \} ⊢ τ}
 @${Δ ⊢ (∀_f (@repeated{@ρf}) τ)}
 @${@textsc{TE-FAll}}
 ]
}

@$${
 @$inferrule[
 @${@ρf ∈ Δ \\ \{@repeated{@|ɐ|ᵢ}\} n \{@repeated{@|ɐ|ⱼ}\} = ∅ \\ @alldifferent(@repeated{@|ɐ|ᵢ}) \\ @alldifferent(@repeated{@|ɐ|ⱼ})}
 @${Δ ⊢ @record[@ρf @repeatset{-@|ɐ|ᵢ} @repeatset{+@|ɐ|ⱼ:τⱼ}]}
 @${@textsc{TE-FXYZ}}
 ]
}

where

@$${
 \begin{aligned}
 @alldifferent(@repeated{y}) &= @metatrue @textif ∀ i ≠ j . yᵢ ≠ yⱼ \\
 @alldifferent(@repeated{y}) &= @metafalse @otherwise
 \end{aligned}
}

@;{TODO: if we extend rows with subtraction, we may need to allow it either in
 function types or within bodies too.
   
 @$${
  @$inferrule[
 @${Δ ∪ \{ @ρc \} ⊢ τ}
 @${Δ ⊢ ???}
 @${@textsc{TE-CFun}}
 ]
 }

 @$${
  @$inferrule[
 @${Δ ∪ \{ @ρf \} ⊢ τ}
 @${Δ ⊢ ???}
 @${@textsc{TE-FFun}}
 ]
 }
}

@$${
 @$inferrule[
 @${@ρc ∈ Δ \\ @repeated{Δ ⊢ τᵢ}}
 @${Δ ⊢ @variant[@repeated{@ctor[@|κ|ᵢ τᵢ]} @ρc]}
 @${@textsc{TE-CVariant}}
 ]
}

@$${
 @$inferrule[
 @${@ρf ∈ Δ \\ @repeated{Δ ⊢ τᵢ}}
 @${Δ ⊢ @record[@repeated{@|ɐ|ᵢ : τᵢ} @ρf]}
 @${@textsc{TE-FRecord}}
 ]
}

