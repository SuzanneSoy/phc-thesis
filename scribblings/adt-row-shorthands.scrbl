#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Shorthands (with ρ)}

The polymorphic builder function for the @κ constructor which
intuitively corresponds to @ctore[κ] can be written as the η-expansion of the
@ctore[κ e] operator:

@$${@Λe[(α) @λe[(@${x : α}) @ctore[κ x]]]}

The same applies to the predicate form of constructors:

@$${@λe[(@${x : ⊤}) @app[@ctor-pred[κ] x]]}

The same applies to the accessor for a constructor's encapsulated value:

@$${@Λe[(α) @λe[(@${x : @ctorτ[κ α]}) @app[@ctor-val[κ] x]]]}

As a convenience, we will write @ctore[κ], @ctor-pred[κ] and @ctor-val[κ] as a
shorthand for the above lambda functions.

As per the typing rules given in @secref{adt-row-trules}, these functions have
the following types:

@htodo{The rules below are the same??? Probably an unfinished copy-paste}

@$inferrule[-
            @${Γ ⊢ @ctorτ[κ] : @∀r[(α) @f→[(α) @R[@ctorτ[κ α] ϵ ⊥ ∅]]]}
            @${@textsc{T-Shorthand-Ctor}}]

@todo{Write their types here too.}


The polymorphic builder function for a record which
intuitively corresponds to @recorde[@repeated{@ɐ}] can be written as the
η-expansion of the @recorde[@repeated{@ɐ = e}] operator:

@$${
 @Λe[(@repeated{αᵢ}) @λe[(@repeated{xᵢ : αᵢ}) @recorde[@repeated{@|ɐ|ᵢ = xᵢ}]]]
}

The same applies to the predicate forms of record types:

@;{
 @$${
  @aligned{
   &(λ ([x : ⊤])\\
   &\quad(Λ (@repeated{αᵢ})\\
   &\qquad(λ (@repeated{[pᵢ : (⊤ \xrightarrow[∅]{αᵢ|\overline{αᵢ}} Boolean)]})\\
   &\qquad\quad(@record-pred[@repeatset{@|ɐ|ᵢ ? pᵢ}] x))}\\
 }
}

@$${
 \begin{gathered}
 @λe[(@${x : ⊤}) @app[@record-pred[@repeatset{@|ɐ|ᵢ}] x]] \\
 @λe[(@${x : ⊤}) @app[@record-pred*[@repeatset{@|ɐ|ᵢ} @repeatset{-@|ɐ|ⱼ}] x]]
 \end{gathered}
}

The same applies to the accessor for a field of a record:

@$${@Λe[(α) @Λfe[(@ρf) @λe[(@${x : @recorde[@ρf @${+@|ɐ|:τ}]})
                           @record-gete[x @ɐ]]]]}

@todo{Write their types here too.}

As a convenience, we will write @recorde[@repeated{@ɐ}],
@record-pred[@repeatset{@|ɐ|ᵢ}],
@record-pred[@repeatset{@|ɐ|ᵢ} @repeatset{-@|ɐ|ⱼ}] and @record-gete[ɐ] as
shorthands for the above lambda functions.
