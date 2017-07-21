#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Shorthands (with ρ)}

The polymorphic builder function for the @κ constructor which
intuitively corresponds to @ctor[κ] can be written as the η-expansion of the
@ctor[κ e] operator:

@$${(Λ (α) (λ ([x : α]) @ctor[κ x]))}

The same applies to the predicate form of constructors:

@$${(λ ([x : ⊤]) (@ctor-pred[κ] x))}

The same applies to the accessor for a constructor's encapsulated value:

@$${(Λ (α) (λ ([x : @ctor[κ α]]) (@ctor-val[κ] α))}

As a convenience, we will write @ctor[κ], @ctor-pred[κ] and @ctor-val[κ] as a
shorthand for the above lambda functions.

As per the typing rules given in @secref{adt-row-trules}, these functions have
the following types:

@$${
 @$inferrule[
 @${@textsc{T-Abs}}
 @${Γ ⊢ @ctor[κ] : (∀ (α) (α → @ctor[κ α])) ; ϵ|⊥ ; ∅}
 ]
}

@$${
 @$inferrule[
 @${…}
 @${Γ ⊢ @ctor[κ] : (∀ (α) (α → @ctor[κ α])) ; ϵ|⊥ ; ∅}
 ]
}

@todo{Write their types here too.}


The polymorphic builder function for a record which
intuitively corresponds to @record[@repeated{@ɐ}] can be written as the
η-expansion of the @record[@repeated{@ɐ = e}] operator:

@$${
 (Λ (@repeated{αᵢ}) (λ (@repeated{[xᵢ : αᵢ]}) @record[@repeated{@|ɐ|ᵢ = xᵢ}]))
}

The same applies to the predicate form of records:

@;{
 @$${
  @aligned{
   &(λ ([x : ⊤])\\
   &\quad(Λ (@repeated{αᵢ})\\
   &\qquad(λ (@repeated{[pᵢ : (⊤ \xrightarrow[∅]{αᵢ|\overline{αᵢ}} Boolean)]})\\
   &\qquad\quad(@record-pred[@repeated{@|ɐ|ᵢ ? pᵢ}] x))}\\
 }
}

@$${
 @aligned{
  &(λ ([x : ⊤])\\
  &\quad(@record-pred[@repeated{@|ɐ|ᵢ}] x))}\\
}

The same applies to the accessor for a constructor's encapsulated value:

@$${(Λ (α) (λ ([x : @ctor[κ α]]) (@ctor-val[κ] α))}

@todo{Write their types here too.}

As a convenience, we will write @ctor[κ], @ctor-pred[κ] and @ctor-val[κ] as a
shorthand for the above lambda functions.

