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
      #`@${(@(add-between (list (string-append "\\textbf{" #,op "}")
                                #,@(syntax->list #'(→str ...)))
                          "\\ "))}]))
@(define-syntax stringify
   (syntax-parser
     [(_ :stringify)
      #'→str]))

@(define-syntax ctor (defop "ctor"))
@(define ↄ @${ↄ})
@(define-syntax ctorTop (defop "CtorTop"))
@(define-syntax ctor-val (defop "ctor"))
@(define-syntax record (defop "record"))
@(define-syntax variant (defop "V"))
@(define (repeated . l) @${\overrightarrow{@l}})
@(define |P| @${\ |\ })
@(define ρc @${\rho_{c}})
@(define ρf @${\rho_{f}})
@(define-syntax atc (defop "@${}_{\\textbf{c}}$"))
@(define-syntax atf (defop "@${}_{\\textbf{f}}$"))
@(define-syntax ctor-pred (defop "ctor-pred"))
@(define-syntax-rule (opwith rec a v)
   @list{@(stringify rec) \textbf{ with } @(stringify a) = @(stringify v)})
@(define-syntax-rule (opwithout rec a)
   @list{@(stringify rec) \textbf{ without } @(stringify a)})

We define the universe of constructor names @${𝒞} as being equivalent to the
set of strings of unicode characters@htodo{Check in the implementation that
 this is not equivalent to the set of symbols, as these cannot be serialised.},
and the universe of field names @${ℱ} likewise (the distinction is purely in
the way that they are used). Constructor and field names are compile-time
constants, i.e. they are written literally in the program source.

@$${@ↄ ⩴ name ∈ 𝒞}
@$${ɐ ⩴ name ∈ ℱ}

Expressions:

@$${
 @cases["e" #:first-sep "⩴"
        @acase{…}
        @acase{@ctor[@ↄ e]}
        @acase{(@ctor-pred[@ↄ] e)}
        @acase{@record[@repeated{ɐᵢ ↦ eᵢ}]}
        @acase{e.ɐ}
        @acase{@opwith[e ɐ e]}
        @acase{@opwithout[e ɐ]}]
}

Note: In the @${@record[@repeated{ɐᵢ ↦ eᵢ}]} expression, which builds a new
record value, the @${ɐᵢ} are ordered, and the field order defines the order of
evaluation, as indicated by the extensions to the @${E} contexts which is
given below. In other uses, the order of fields within the record is
irrelevant, i.e. the record can be assimilated to a set of 2-uples, of which the
first element is a field name, and the second a value or type.

Values:

@$${v ⩴ … @P @ctor[@ↄ v] @P @record[@repeated{ɐᵢ ↦ vᵢ}]}

Primitive operations:

@$${c ⩴ … @P \textit{ctor-val}}

Evaluation contexts:

@$${
 @cases["E" #:first-sep "⩴"
        @acase{…}
        @acase{@ctor[@ↄ E]}
        @acase{(@ctor-pred[@ↄ] E)}
        @acase{@record[@repeated{ɐᵢ ↦ vᵢ} ɐⱼ ↦ E @repeated{ɐₖ ↦ eₖ}]}
        @acase{@opwith[E ɐ e]}
        @acase{@opwith[v ɐ E]}
        @acase{@opwithout[E ɐ]}]
}

Types:

@$${σ,τ ⩴ … @P @ctor[@ↄ τ] @P @ctorTop[τ] @P @record[@repeated{ɐᵢ ↦ τᵢ}]}

Subtyping:

@$${
 @$inferrule[
 @${\phantom{x}}
 @${⊢ @ctor[@ↄ τ] <: @ctorTop[τ]}
 @${@textsc{S-CtorTop}}
 ]
}

@$${
 @$inferrule[
 @${⊢ τ <: τ'}
 @${⊢ @ctor[@ↄ τ] <: @ctor[@ↄ @${τ'}]}
 @${@textsc{S-Ctor}}
 ]
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
 @repeated{@ctor[@ↄᵢ τᵢ]}
 @;@repeated{@record[@repeated{ɐⱼₗ ↦ τⱼₗ}]}
 @;@${τ_{last}}
 ]@;\\
 @;@where \{@repeated{ɐⱼₗ}\} ≠ \{@repeated{ɐₖₗ}\} ∀ j ≠ k
 \end{gathered}
}

Path elements:

@$${pe ⩴ … @P \textbf{ctor-val} @P ɐ}


Typing rules:

@todo{Should the filter be something else than @${ϵ|ϵ} or is the filter inferred
 via other rules when the ``function'' does not do anything special?}

@$${
 δ_τ(\textit{ctor-val})
 = (∀ (τ) @ctorTop[τ] \xrightarrow[\textbf{ctor-val}]{ϵ|ϵ} τ)
}

@;{
 @$${
  @$inferrule[
 @${Γ ⊢ e : τ ; φ ; o \\ τ <: @ctor[@ↄ @${τ'}]}
 @${Γ ⊢ (\textit{ctor-val} e) : τ' ; ϵ|ϵ ; \textbf{ctor-val}(o)}
 @${@textsc{T-Ctor-Val}}
 ]
 }
}

@$${
 @$inferrule[
 @${Γ ⊢ e : τ ; φ ; o \\ τ <: @record[@repeated{ɐᵢ ↦ τᵢ}]}
 @${Γ ⊢ e.ɐⱼ : τ' ; ϵ|ϵ ; \textbf{ɐⱼ}(o)}
 @${@textsc{T-Record-Get}}
 ]
}

@$${
 @$inferrule[
 @${
   Γ ⊢ e_{r} : τ_{r} ; φ_{r} ; o_{r} \\
   τ_{r} <: @record[@repeated{ɐᵢ ↦ τ'ᵢ}] \\
   Γ ⊢ e_{v} : τ_{v} ; φ_{v} ; o_{v} \\
   ɐ ∉ \{ɐᵢ\}
  }
 @${Γ ⊢ @opwith[@${e_{r}} ɐ @${e_{v}}]
   : @record[@repeated{ɐᵢ ↦ τᵢ} @${ɐ ↦ τ_{v}}]
   ; ϵ|ϵ ; ∅}
 @${@textsc{T-Record-With}_1}
 ]
}

@$${
 @$inferrule[
 @${
   Γ ⊢ e_{r} : τ_{r} ; φ_{r} ; o_{r} \\
   τ_{r} <: @record[@repeated{ɐᵢ ↦ τ'ᵢ}] \\
   Γ ⊢ e_{v} : τ_{v} ; φ_{v} ; o_{v} \\
   ɐ = ɐⱼ
  }
 @${Γ ⊢ @opwith[@${e_{r}} ɐ @${e_{v}}]
   : @record[@repeated{ɐᵢ ↦ τᵢ}@${{}^{i≠j}} @${ɐ ↦ τ_{v}}]
   ; ϵ|ϵ ; ∅}
 @${@textsc{T-Record-With}_2}
 ]
}

@$${
 @$inferrule[
 @${
   Γ ⊢ e_{r} : τ_{r} ; φ_{r} ; o_{r} \\
   τ_{r} <: @record[@repeated{ɐᵢ ↦ τ'ᵢ}] \\
   ɐ = ɐⱼ
  }
 @${Γ ⊢ @opwith[@${e_{r}} ɐ @${e_{v}}]
   : @record[@repeated{ɐᵢ ↦ τᵢ}@${{}^{i≠j}}]
   ; ϵ|ϵ ; ∅}
 @${@textsc{T-Record-Without}}
 ]
}

Semantics:

@$${
 @aligned{
  δ(\textit{ctor-val}, @ctor[@ↄ v]) & = v
 }
}

@$${
 @$inferrule[
 @${\phantom{x}}
 @${(@ctor-pred[@ↄ] @ctor[@ↄ v]) ↪ \textit{\#t}}
 @${@textsc{E-Ctor-Pred-True}}
 ]
}

@todo{The @${@ctor-pred[@ↄ]} should be a value too, and be a procedure type. Or
 we can say that the user manually wraps it in a λ if they need to pass it
 around as a value.}

@$${
 @$inferrule[
 @${∄ v \text{ such that } w = @ctor[@ↄ v]}
 @${(@ctor-pred[@ↄ] w) ↪ \textit{\#f}}
 @${@textsc{E-Ctor-Pred-False}}
 ]
}

@$${
 @$inferrule[
 @${\phantom{x}}
 @${@opwith[E ɐ e] ↪ v}
 @${@textsc{E-Record-With}}
 ]
}

@;{
 @$${
  @$inferrule[
 @${a \\ b}
 @${c \\ d(e)}
 @${@textsc{Foo}_y}
 ]
 }

 @asection{
  @atitle{With row polymorphism}

  Types:

  @$${σ,τ ⩴ …
   @P (∀_c (@repeated{@ρc}) τ)
   @P (∀_c (@repeated{@ρf}) τ)
   @P @variant[@repeated{@ctor[@ↄᵢ τᵢ]} @ρc]
   @P @record[@repeated{ɐᵢ ↦ τᵢ} @ρf]}

  Elements:

  @$${e ⩴ … @P @atc[e @repeated{@ρc}] @P @atf[e @repeated{@ρf}]}
 }
}