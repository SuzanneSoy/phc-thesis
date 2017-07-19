#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Path elements}

@$${pe ⩴ … @P @πctor-val @P @|ɐ|}

We extend the metafunctions for paths given
in@~cite[#:precision "pp. 65 and 75" "tobin-hochstadt_typed_2010"]. The @${
 @update} metafunction is used when using filters to restrict the type
of a (subpart of a) local variable in the @tt{then} and @tt{else} branches of
a conditional.

@todo{∷ is not defined in@~cite["tobin-hochstadt_typed_2010"], we have to
 define it.}

@todo{How should I note cleanly these removals / replacements which refer to
 an ɐ and its τ or @${v} inside the set of @${@|ɐ|ᵢ}?}

@$${
 @aligned{
  @update(@record[@repeated{@|ɐ|ᵢ : τᵢ}ⁿ], υ_{π∷@|ɐ|ⱼ})
  &= @record[@${@repeated{@|ɐ|ᵢ : τᵢ} ∖ \{@|ɐ|ⱼ : τⱼ\}}
            @${@|ɐ|ⱼ : @${@update(τⱼ, υ_π)}}]
  \\
  &\quad @where @|ɐ|ⱼ : τⱼ ∈ @repeatset{@|ɐ|ᵢ : τᵢ}\\
  @update(@ctor[@κ τ], υ_{π∷@πctor-val})
  &= @ctor[@κ @${@update(τ, υ_π)}]
 }
}