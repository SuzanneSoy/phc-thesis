#lang scribble/manual

@require["util.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)
       #:tag "adt-chap"]{Extension of Typed Racket with algebraic
 datatypes and row polymorphism}

We extend the formalisation
from@~cite[#:precision "pp.62, 72 and 92" "tobin-hochstadt_typed_2010"]:

@(require racket/list
          (for-syntax racket/base syntax/parse racket/list))
@(begin-for-syntax
   (define-syntax-class stringify
     [pattern x:id #:with →str (datum->syntax #'x
                                              (symbol->string (syntax-e #'x))
                                              #'x)]
     [pattern e #:with →str #'e]))
@(define-for-syntax (defop op)
   (syntax-parser
     [(_ :stringify ...)
      #`@${(\mathbf{@#,op}\ @(add-between (list #,@(syntax->list #'(→str ...)))
                                          "\\ "))}]))

@(define-syntax ctor (defop "ctor"))
@(define-syntax record (defop "record"))
@(define-syntax variant (defop "V"))
@(define (repeated . l) @${\overrightarrow{@l}})
@(define |P| @${\ |\ })
@(define ρc @${\rho_{c}})
@(define ρf @${\rho_{f}})
@(define atc @${@"@"_{c}})
@(define atf @${@"@"_{f}})

We define the universe of constructor names @${𝒞} as being equivalent to the
set of strings of unicode characters@htodo{Check in the implementation that
 this is not equivalent to the set of symbols, as these cannot be serialised.},
and the universe of field names @${ℱ} likewise (the distinction is purely in
the way that they are used). Constructor and field names are compile-time
constants, i.e. they are written literally in the program source.

@$${ↄ ⩴ name ∈ 𝒞}
@$${ɐ ⩴ name ∈ ℱ}

@$${e ⩴ … @P @ctor[ↄ e] @P @record[@repeated{ɐᵢ ↦ eᵢ}]}

Values:

@$${v ⩴ … @P @ctor[ↄ v] @P @record[@repeated{ɐᵢ ↦ vᵢ}]}

The primitive operations are not modified.

Evaluation contexts:

@$${E ⩴ …
 @P @ctor[ↄ E]
 @P @record[@repeated{ɐᵢ ↦ vᵢ} ɐⱼ ↦ E @repeated{ɐₖ ↦ eₖ}]}

Types:

@$${σ,τ ⩴ … @P @ctor[ↄ τ] @P @record[@repeated{ɐᵢ ↦ τᵢ}]}

With row polymorphism;:

@$${σ,τ ⩴ …
 @P (∀_c (@repeated{@ρc}) τ)
 @P (∀_c (@repeated{@ρf}) τ)
 @P @ctor[ↄ τ @ρc]
 @P @record[@repeated{ɐᵢ ↦ τᵢ} @ρf]}

@$${e ⩴ … @P (@atc e @repeated{@ρc}) @P (@atf e @repeated{@ρf})}

We further define variants as a subset of the unions allowed by @|typedracket|
(including unions of the constructors defined above). Variants are equivalent
to the union of their cases, but guarantee that pattern matching can always be
performed (for example, it is not possible in @|typedracket| to distinguish the
values of two function types present in the same union).

@todo{What about rows here?}
@$${
 \begin{gathered}
 σ,τ ⩴ …
 @P @variant[
  @repeated{@ctor[ↄᵢ τᵢ]}
  @repeated{@record[@repeated{ɐⱼₗ ↦ τⱼₗ}]}
  @${τ_{last}}]\\
 @where \{ɐⱼₗ …\} ≠ \{ɐₖₗ …\} ∀ j ≠ k
 \end{gathered}
}

Path elements:

@$${pe ⩴ … @P \mathbf{ctor-val} @P ɐ}


Typing rules:

@$${
 @$inferrule[
 @${a \\ b}
 @${c \\ d(e)}
 @${@textsc{Foo}_y}
 ]
}