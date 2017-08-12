#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Expressions (with ρ)}

We extend the syntax of expressions in typed racket as defined
by@~cite[#:precision "pp. 62, 72 and 92" "tobin-hochstadt_typed_2010"] by
adding expressions related to constructors. The first syntax builds an
instance of the constructor with label @|κ| and the value of @${e}. The
expression @${(@ctor-pred[@κ] e)} determines whether @${e} is an instance of
the constructor with label @|κ|. The expression @${(@ctor-val[@κ] e)} extracts
the value stored in an instance of a constructor.

@cases*[
 "e" #:first-sep "⩴"
 @acase{…}
 @acase{@ctor[@κ e]}
 @acase{(@ctor-pred[@κ]\ e)}
 @acase{(@ctor-val[@κ]\ e)}
 @intertext{@list[]
            
  We also introduce expressions related to records. The first builds an instance
  of a record with the given fields. We note that the order in which the fields
  appear affects the order in which the sub-expressions will be evaluated.
  However, in the resulting value and in its type, the order of the fields is
  irrelevant. The second expression tests whether @${e} is an instance of a
  record with the given fields present. The third expression is similar, but
  allows any number of extra fields to be present, while checking that the @${
   -@|ɐ|ⱼ} fields are absent. The fourth expression accesses the value of the @ɐ
  field stored in the given instance. The fifth expression updates an existing
  record instance by adding (or replacing) the field @ɐ, while the sixth removes
  the @ɐ field.

 @list[]}

 @acase{…}
 @acase{@record[@repeated{@↦e[@${@|ɐ|ᵢ} eᵢ]}]}
 @acase{(@record-pred[@repeated{@|ɐ|ᵢ}]\ e)}
 @acase{(@record-pred*[@repeated{@|ɐ|ᵢ} @repeated{-@|ɐ|ⱼ}]\ e)}@;added
 @acase{e.@|ɐ|}
 @acase{@opwith[e @|ɐ| e]}
 @acase{@opwithout[e @|ɐ|]}

 @intertext{@list[]
           
  Finally, we define the row-polymorphic abstractions
  @Λc[@${(@repeated{@ρc})} e] and @Λf[@${(@repeated{@ρf})} e] which bind row
  type variables hiding constructors and fields respectively. The corresponding
  instantiation operators are @atc[e @repeated{@ρc}] and @atf[e @repeated{@ρf}].
   
 @list[]}

 @acase{…}
 @acase{@Λc[@${(@repeated{@ρc})} e]}@; new
 @acase{@Λf[@${(@repeated{@ρf})} e]}@; new
 @acase{@atc[e @repeated{@ρc}]}@; new
 @acase{@atf[e @repeated{@ρf}]}@; new
 ]


@;{
 Note: In the @${@record[@repeated{@|ɐ|ᵢ = eᵢ}]} expression, which builds a
 new record value, the @${@|ɐ|ᵢ} are ordered, and the field order defines the
 order of evaluation, as indicated by the extensions to the @${E} contexts
 which is given below. In other uses, the order of fields within the record is
 irrelevant, i.e. the record can be assimilated to a set of 2-uples, of which
 the first element is a field name, and the second a value or type.
}
