#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         scriblib/render-cond
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Path elements (with ρ)}

@todo{Does this need any change when adding row typing?}

@cases[@pe #:first-sep "⩴"
       @acase{…}
       @acase{@πctor-val @tag*{value of constructor}}
       @acase{@|ɐ| @tag*{value of field @|ɐ|}}]

We extend the metafunctions for paths given
in@~cite[#:precision "pp. 65 and 75" "tobin-hochstadt_typed_2010"]. The @${
 @update} metafunction is used when using filters to restrict the type
of a (subpart of a) local variable in the @tt{then} and @tt{else} branches of
a conditional.

@todo{∷ is not defined in@~cite["tobin-hochstadt_typed_2010"], we have to
 define it.}

@todo{How should I note cleanly these removals / replacements which refer to
 an @|ɐ| and its τ or @${v} inside the set of @${@|ɐ|ᵢ}?}

@todo{Also write down the simple "update" cases for row polymorphism}

@aligned{
 @update(@recordτ[@repeatset{@|ɐ|ᵢ : τᵢ}], σ_{π∷@|ɐ|ⱼ})
 &= @recordτ[@${@repeatset{@|ɐ|ᵢ : τᵢ} ∖ \{@|ɐ|ⱼ : τⱼ\}}
             @${@|ɐ|ⱼ : @${@update(τⱼ, σ_π)}}]
 \\
 @update(@recordτ[@repeatset{@|ɐ|ᵢ : τᵢ}], @!{σ}_{π∷@|ɐ|ⱼ})
 &= @recordτ[@${@repeatset{@|ɐ|ᵢ : τᵢ} ∖ \{@|ɐ|ⱼ : τⱼ\}}
             @${@|ɐ|ⱼ : @${@update(τⱼ, @!{σ}_π)}}]
 \\
 &\quad @where @|ɐ|ⱼ : τⱼ ∈ @repeatset{@|ɐ|ᵢ : τᵢ}\\
 @update(@ctorτ[@κ τ], σ_{π∷@πctor-val})
 &= @ctorτ[@κ @${@update(τ, σ_π)}]\\@;TODO: update rule when the π is ɐ or getval
}

@$${
 @restrict(@variantτ[@repeatset{τ}], σ)
 = @variantτ[@repeatset{@restrict(τ,σ)}]
}
@$${
 @cond-element[
 [html @${\begin{multline}}]
 [else @${\begin{multlined}}]]
 @|restrict|\left(\begin{aligned}
 &@recordτ[@ρf
            @repeatset{-@|ɐ|ᵢ}
            @repeatset{-@|ɐ|ⱼ}
            @repeatset{+@|ɐ|ₗ : τₗ}
            @repeatset{+@|ɐ|ₘ : τₘ}],
 \\@; TODO: this does not handle the regular per-field update like above?
 &@recordτ[@ρf
            @repeatset{-@|ɐ|ᵢ}
            @repeatset{-@|ɐ|ₖ}
            @repeatset{+@|ɐ|ₗ : σₗ}
            @repeatset{+@|ɐ|ₙ : σₙ}]\end{aligned}\right)
 \\
 = @recordτ[@repeatset{-@|ɐ|ᵢ}
            @repeatset{-@|ɐ|ⱼ}
            @repeatset{-@|ɐ|ₖ}
            @repeatset{+@|ɐ|ₗ : @update(τₗ, σₗ)}
            @repeatset{+@|ɐ|ₘ : τₘ}
            @repeatset{+@|ɐ|ₙ : σₙ}]
 @cond-element[
 [html @${\end{multline}}]
 [else @${\end{multlined}}]]
}
  
@$${
 @where @disjoint-sets(
 @repeatSet{@|ɐ|ᵢ},
 @repeatSet{@|ɐ|ⱼ},
 @repeatSet{@|ɐ|ₖ},
 @repeatSet{@|ɐ|ₗ},
 @repeatSet{@|ɐ|ₘ},
 @repeatSet{@|ɐ|ₙ})
}

where

@aligned{
 @disjoint-sets(@repeated{s})
 &= @metatrue @textif ∀ sᵢ, sⱼ ∈ s . i ≠ j ⇒ sᵢ ∩ sⱼ = ∅\\
 @disjoint-sets(@repeated{s})
 &= @metafalse @otherwise
}

@aligned{
 @remove(@variantτ[@repeatset{τ}], σ) &= @variantτ[@repeatset{@remove(τ,σ)}]
}
