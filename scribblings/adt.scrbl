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

 @$${c ⩴ … @P @textit{ctor-val}}
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

@asection{
 @atitle{With row polymorphism}
 @include-asection["adt-row-e.scrbl"]
 @include-asection["adt-row-v.scrbl"]
 @include-asection["adt-row-ectx.scrbl"]
 @include-asection["adt-row-ty.scrbl"]
 @include-asection["adt-row-sub.scrbl"]
 @include-asection["adt-row-pe.scrbl"]
 @include-asection["adt-row-trules.scrbl"]
 @include-asection["adt-row-opsem.scrbl"]
 @include-asection["adt-row-shorthands.scrbl"]
}


@todo{Argue that a type system with symbols, unique tokens (a way to create a
 fresh "identity", like (cons 0 0) creates a fresh memory cell which is not eq?
 to any other, past or future), untagged unions, pure case→ functions with the
 usual polymorphism and row polymorphism (ranging over function cases, with
 negation), i.e. a simplification / generalisation of our system, is more
 "general" than @typedracket (i.e. Racket's existing types can be easily
 encoded with ours), and show how other systems of variants and records can be
 encoded with it (try the one from @CAML + the "backtick" cases of variants;
 try to see if the one from tinybang is more general, less or different).}