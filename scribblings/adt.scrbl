#lang scribble/manual

@require["util.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)
       #:tag "adt-chap"]{Extension of Typed Racket with algebraic
 datatypes and row polymorphism}

We extend the formalisation
from@~cite[#:precision "pp. 62, 72 and 92" "tobin-hochstadt_typed_2010"]:

@require["adt-utils.rkt"]

We define the universe of constructor names @${𝒞} as being equivalent to the
set of strings of unicode characters@htodo{Check in the implementation that
 this is not equivalent to the set of symbols, as these cannot be serialised.},
and the universe of field names @${ℱ} likewise (the distinction is purely in
the way that they are used). Constructor and field names are compile-time
constants, i.e. they are written literally in the program source.

@$${@κ ⩴ name ∈ 𝒞}
@$${@ɐ ⩴ name ∈ ℱ}

@include-asection["adt-e.scrbl"]
@include-asection["adt-v.scrbl"]
@include-asection["adt-ectx.scrbl"]
@include-asection["adt-ty.scrbl"]
@include-asection["adt-sub.scrbl"]
@include-asection["adt-pe.scrbl"]
@include-asection["adt-trules.scrbl"]
@include-asection["adt-opsem.scrbl"]

@;{
 Primitive operations:

 @$${c ⩴ … @P \textit{ctor-val}}
}

We further define variants as a subset of the unions allowed by @|typedracket|
(including unions of the constructors defined above). Variants are equivalent
to the union of their cases, but guarantee that pattern matching can always be
performed (for example, it is not possible in @|typedracket| to distinguish the
values of two function types present in the same union).

@$${
 \begin{gathered}
 σ,τ ⩴ …
 @P @variant[
 @repeated{@ctor[@|κ|ᵢ τᵢ]}
 @;@repeated{@record[@repeated{@|ɐ|ⱼₗ : τⱼₗ}]}
 @;@${τ_{last}}
 ]@;\\
 @;@where \{@repeated{@|ɐ|ⱼₗ}\} ≠ \{@repeated{@|ɐ|ₖₗ}\} ∀ j ≠ k
 \end{gathered}
}

Notes: the polymorphic builder function for the @κ constructor which
intuitively corresponds to @ctor[κ] can be written as the η-expansion of the
@ctor[κ e] operator:

@$${(Λ (α) (λ ([x : α]) @ctor[κ x]))}

The same applies to the predicate form of constructors:

@$${(λ ([x : ⊤]) (@ctor-pred[κ] x))}

The same applies to the accessor for a constructor's encapsulated value:

@$${(Λ (α) (λ ([x : @ctor[κ α]]) (@ctor-val[κ] α))}

@todo{Write their types here too.}

As a convenience, we will write @ctor[κ], @ctor-pred[κ] and @ctor-val[κ] as a
shorthand for the above lambda functions.


The polymorphic builder function for a record which
intuitively corresponds to @record[@repeated{ɐ}] can be written as the
η-expansion of the @record[@repeated{ɐ = e}] operator:

@$${(Λ (@repeated{αᵢ}) (λ (@repeated{[xᵢ : αᵢ]}) @record[@repeated{ɐᵢ = xᵢ}]))}

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




@asection{
 @atitle{With row polymorphism}

 Types:

 @$${σ,τ ⩴ …
  @P (∀_c (@repeated{@ρc}) τ)
  @P (∀_c (@repeated{@ρf}) τ)
  @P @variant[@repeated{@ctor[@|κ|ᵢ τᵢ]} @ρc]
  @P @record[@repeated{@|ɐ|ᵢ : τᵢ} @ρf]}

 Elements:

 @$${e ⩴ … @P @atc[e @repeated{@ρc}] @P @atf[e @repeated{@ρf}]}

 @include-asection["adt-e.scrbl"]
 @include-asection["adt-v.scrbl"]
 @include-asection["adt-ectx.scrbl"]
 @include-asection["adt-ty.scrbl"]
 @include-asection["adt-sub.scrbl"]
 @include-asection["adt-pe.scrbl"]
 @include-asection["adt-trules.scrbl"]
 @include-asection["adt-opsem.scrbl"]
}