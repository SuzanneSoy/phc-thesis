#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Expressions (with ρ)}

We extend the syntax of expressions in typed racket as defined
by@~cite[#:precision "pp. 62, 72 and 92" "tobin-hochstadt_typed_2010"] and
presented in
@secref["from-dissertation-tobin-hochstadt"
        #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")]
by adding expressions related to constructors. The first syntax builds an
instance of the constructor with label @|κ| and the value of @${e}. The
expression @${(@ctor-pred[@κ] e)} determines whether @${e} is an instance of
the constructor with label @|κ|. The expression @${(@ctor-val[@κ] e)} extracts
the value stored in an instance of a constructor.

@cases*[
 "e" #:first-sep "⩴"
 @acase{…}
 @acase{@ctore[@κ e]@tag*{constructor instance}}
 @acase{(@ctor-pred[@κ]\ e)@tag*{constructor predicate}}
 @acase{(@ctor-val[@κ]\ e)@tag*{constructor value access}}
 @interpar{
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
 }
 @acase{…}
 @acase{@recorde[@repeated{@↦e[@${@|ɐ|ᵢ} eᵢ]}]@tag*{record instance}}
 @acase{(@record-pred[@repeatset{@|ɐ|ᵢ}]\ e)@tag*{record predicate}}
 @acase{(@record-pred*[@repeatset{@|ɐ|ᵢ} @repeatset{-@|ɐ|ⱼ}]\ e)
  @tag*{row-record predicate}}@;added
 @acase{@record-gete[e @|ɐ|]@tag*{record field access}}
 @acase{@opwith[e @|ɐ| e]@tag*{record update (new/change field)}}
 @acase{@opwithout[e @|ɐ|]@tag*{record update (remove field)}}
 @interpar{
  Finally, we define the row-polymorphic abstractions
  @Λce[(@repeated{@ρc}) e] and @Λfe[(@repeated{@ρf}) e] which bind row
  type variables hiding constructors and fields respectively. The corresponding
  instantiation operators are @atc[e @repeated{@ςc}] and @atf[e @repeated{@ςf}].
 }
 @acase{…}
 @acase{@Λce[(@repeated{@ρc}) e]
  @tag*{row-polymorphic abstraction (constructors)}}@; new
 @acase{@Λfe[(@repeated{@ρf}) e]
  @tag*{row-polymorphic abstraction (fields)}}@; new
 @acase{@atc[e @repeated{@ςc}]
  @tag*{row-polymorphic instantiation (constructors)}}@; new
 @acase{@atf[e @repeated{@ςf}]
  @tag*{row-polymorphic instantiation (fields)}}]@; new

@;{
 Note: In the @${@record[@repeated{@|ɐ|ᵢ = eᵢ}]} expression, which builds a
 new record value, the @${@|ɐ|ᵢ} are ordered, and the field order defines the
 order of evaluation, as indicated by the extensions to the @${E} contexts
 which is given below. In other uses, the order of fields within the record is
 irrelevant, i.e. the record can be assimilated to a set of 2-uples, of which
 the first element is a field name, and the second a value or type.
}
