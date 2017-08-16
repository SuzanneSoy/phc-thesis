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
        @acase{@ctor[@κof[τ]]} @; same
        @acase{@record[@ςf]} @; changed
        @acase{@∀c[@${(@repeated{@ρc})} τ]} @; new
        @acase{@∀f[@${(@repeated{@ρf})} τ]}] @; new
}

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
       @acase{@variant[@ςf]}] @; new/changed

@; TODO: what about adding some fields to the ρ used to instantiate another function?
@; new
@cases[@ςc #:first-sep "⩴"
       @acase{@repeatset{@κof[τ]}}
       @acase{@ρc @repeatset{-@|κ|ᵢ} @repeatset{+@κof[ⱼ τⱼ]}
        @;@where \{@repeated{@|κ|ᵢ}\} ≠ \{@repeated{@|κ|ⱼ}\} ∀ i ≠ j
        }]

@todo{Notation to indicate that all present and absent fields (resp.
 constructors) are different, including between the two (present vs absent)
 sets.}

@cases[@ςf #:first-sep "⩴"
       @acase{@repeatset{@|ɐ|:τ}}
       @acase{@ρf @repeatset{-@|ɐ|ᵢ} @repeatset{+@|ɐ|ⱼ:τⱼ}
        @;@where \{@repeated{@|ɐ|ᵢ}\} ≠ \{@repeated{@|ɐ|ⱼ}\} ∀ i ≠ j
        }]
