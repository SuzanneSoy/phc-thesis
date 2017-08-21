#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Types (with ρ)}

We introduce two new sorts of types: constructors and records. Constructors
are similar to a @typedracket pair whose left element is a symbol, used as a
tag. A constructor's tag does not need to be declared beforehand, and can be
used on-the-fly. This makes these constructors very similar to the
constructors used in @OCAML's polymorphic
variants@~cite[#:precision "chapter 6" "minskyRealWorldOCaml"]. Records are
similar to the @racket[struct]s available in @typedracket, but the accessor
for a field is not prefixed by the name of the struct introducing that field.
This means that the same accessor can be used to access the field in all
records which contain that field, whereas a different accessor would be needed
for each struct type in @|typedracket|. We also introduce row-polymorphic
abstractions, which allow a single row type variable to range over several
constructors or fields.

@cases["σ,τ" #:first-sep "⩴"
       @acase{…}
       @acase{@ctorτ[@κof[τ]]@tag*{constructor}} @; same
       @acase{@recordτ[@ςf]@tag*{possibly row-polymorphic record}} @; changed
       @acase{@∀c[(@repeated{@ρc}) τ]
        @tag*{row-polymorphic abstraction (constructors)}} @; new
       @acase{@∀f[(@repeated{@ρf}) τ]
        @tag*{row-polymorphic abstraction (fields)}}] @; new

@; new↓

We further define variants as a subset of the unions allowed by @|typedracket|
(including unions of the constructors defined above). Variants are equivalent
to the union of their cases, but guarantee that pattern matching can always be
performed (for example, it is not possible in @|typedracket| to distinguish
the values of two function types present in the same union, and it is
therefore impossible to write a pattern matching expression which handles the
two cases differently). Additionally, constraints on the absence of some
constructors in the row type can be specified on variants.

@cases["σ,τ" #:first-sep "⩴"
       @acase{…}
       @acase{@variantτ[@ςf]
        @tag*{possibly row-polymorphic variant}}] @; new/changed

A variant acts as the union of multiple constructor types. The variant type
can also contain a row type variable ranging over constructors. A variant
containing a row type variable will normally contain all the constructors used
to instantiate that row. The constructors which are explicitly marked as
omitted using the syntax @${-@|κ|} are however removed from the row if present
within, and the constructors explicitly marked as present using the syntax @${
 +@κof[τ]} will always be members of the variant. If such a constructor was
present in the row with a different type, it is replaced by a constructor
wrapping a value of the explicitly specified type.

@cases[@ςc #:first-sep "⩴"
       @acase{@repeatset{@κof[τ]}@tag*{fixed constructors}}
       @acase{@|ρc|\ @repeatset{-@|κ|ᵢ}\ @repeatset{+@κof[ⱼ τⱼ]}
        @tag*{row without some ctors, with extra ctors}}]

Similarly, records can contain a set of fields. It is also possible to use a
row type variable ranging over constructors. The syntax @${-@|ɐ|} indicates a
field which could be present in the row and but will not be present in the
record type, and the syntax @${+@|ɐ|:τ} indicates a field which will always be
present, as well as its type.

@cases[@ςf #:first-sep "⩴"
       @acase{@repeatset{@|ɐ|:τ}@tag*{fixed fields}}
       @acase{@|ρf|\ @repeatset{-@|ɐ|ᵢ}\ @repeatset{+@|ɐ|ⱼ:τⱼ}
        @tag*{row without some fields, with extra fields}}]
