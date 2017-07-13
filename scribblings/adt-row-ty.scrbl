#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Types (with ρ)}

@$${
 @cases["σ,τ" #:first-sep "⩴"
        @acase{…}
        @acase{@ctor[@κ τ]} @; same
        @acase{@variant[@repeated{@ctor[@|κ|ᵢ τᵢ]} @ρc]} @; new/changed
        @acase{@record[@repeated{@|ɐ|ᵢ : τᵢ}
                       @repeatset{-@|ɐ|ᵢ}
                       @${@ρf - @repeatset{@|ɐ|ⱼ}}]} @; changed
        @acase{@∀c[@${(@repeated{@ρc})} τ]} @; new
        @acase{@∀f[@${(@repeated{@ρf})} τ]}] @; new
}

@; new↓

Type validity judgements (for well-scopedness):

@todo{Just as TR distinguishes regular @${@repeated{α}} and dotted @${α
  \mathbf{…}} type variables, we should distinguish @${@repeated{α}}, @${α
  \mathbf{…}}, @ρc and @ρf variables (so four different notations.}

@$${
 @$inferrule[
 @${Δ ∪ \{ @ρc \} ⊢ τ}
 @${Δ ⊢ (∀_c (@ρc) τ)}
 @${@textsc{TE-CAll}}
 ]
}

@$${
 @$inferrule[
 @${Δ ∪ \{ @ρf \} ⊢ τ}
 @${Δ ⊢ (∀_f (@ρf) τ)}
 @${@textsc{TE-FAll}}
 ]
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