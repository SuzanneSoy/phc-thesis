#lang scribble/manual

@; This file is not under the CC0 license, as it contains rules and definitions
@; copied with permission from Sam Tobin-Hochstadt's Ph.D thesis. I obtained the
@; permission to copy these rules, but did not ask for a relicensing under the
@; CC0 license.

@require["../scribblings/util.rkt"
         "../scribblings/abbreviations.rkt"
         "../scribblings/adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket)
                    typed/racket/class)
         (only-in scribble/base emph)
         scribble/example
         racket/string]
@(use-mathjax)

@(define tr-eval (make-eval-factory '(typed/racket)))

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)
       #:tag "from-dissertation-tobin-hochstadt"]{Formal semantics for part of
 @|typedracket|'s type system}

The following definitions and rules are copied and adjusted
from@~cite["tobin-hochstadt_typed_2010"], with the author's permission. Some
of the notations were changed to use those of@~cite["kent2016occurrence"].

We include below the grammar, semantics and typing rules related to the
minimal core of the Typed Racket language@note{The core language is defined
 in@~cite[#:precision "pp. 61–70" "tobin-hochstadt_typed_2010"].}, dubbed @${
 λ_{\mathit{TS}}}, including extensions which add pairs@note{The extensions
 needed to handle pairs are described
 in@~cite[#:precision "pp. 71–75" "tobin-hochstadt_typed_2010"].}, functions of
multiple arguments, variadic functions and variadic polymorphic
functions@note{The extensions needed to handle functions of multiple
 arguments, variadic functions, and variadic functions where the type of the
 “rest” arguments are not uniform are described
 in@~cite[#:precision "pp. 91–77" "tobin-hochstadt_typed_2010"].}, intersection
types, recursive types, symbols and promises. These features have been
informally described in @secref["tr-overview"].

We purposefully omit extensions which allow advanced logic reasoning when
propagating information gained by complex combinations of conditionals@note{
 The extensions which allow advanced logic reasoning are described
 in@~cite[#:precision "pp. 75–78" "tobin-hochstadt_typed_2010"].}, refinement
types@note{The extensions which introduce refinement types are described
 in@~cite[#:precision "pp. 85–89" "tobin-hochstadt_typed_2010"].}, dependent
refinement types@note{Dependent refinement types are presented in
 @~cite["kent2016occurrence"].} (which allow using theories from external
solvers to reason about values and their type, e.g. using bitvector theory to
ensure that a sequence of operations does not produce a result exceeding a
certain machine integer size), structs and classes. These extensions are not
relevant to our work@note{We informally describe a translation of our system
 of records into structs in section @todo{[??]}, but settle for an alternative
 implementation in section @todo{[??]} which does not rely on structs.}, and
their inclusion in the following semantics would needlessly complicate things.

@subsubsub*section{Expressions}

The following expressions are available in the subset of @typedracket which we
consider. These expressions include references to variables, creation of basic
values (numbers, booleans, lists of pairs ending with @null-v, symbols,
promises), a variety of lambda functions with different handling of @emph{
 rest} arguments (fixed number of arguments, polymorphic functions with a
uniform list of @emph{rest} arguments and variadic polymorphic functions, as
well as polymorphic abstractions), a small sample of primitive functions which
are part of Racket's library and a few operations manipulating these values
(function application and polymorphic instantiation, forcing promises, symbol
comparison and so on).

@include-equation["e.rkt"]

Symbol literals are noted as @${s ∈ 𝒮} and the universe of symbols (which
includes symbol literals and fresh symbols created via @gensyme[]) is noted as
@${@sym* ∈ @𝒮*}.

@include-equation["e.rkt" sym]

@subsubsub*section{Primitive operations (library functions)}

Racket offers a large selection of library functions, which we consider as
primitive operations. A few of these are listed below, and their type is given
later after, once the type system has been introduced. @textit{number?},
@textit{pair?} and @textit{null?} are predicates for the corresponding type.
@textit{car} and @textit{cdr} are accessors for the first and second elements
of a pair, which can be created using @|consp|. The @textit{identity} function
returns its argument unmodified, and @textit{add1} returns its numeric
argument plus 1. These last two functions are simply listed as examples.

@include-equation["p.rkt"]

@subsubsub*section{Values}

These expressions and primitive functions may produce or manipulate the
following values:

@include-equation["v.rkt"]

@subsubsub*section{Run-time environment}

Lambda functions are closures over their execution environment. The execution
environment maps to their value those variables which were within the scope of
the closure. In principle, it also maps type variables and dotted type
variables to the type or types used to instantiate the polymorphic functions
which are part of the scope of the closure. Typed Racket uses @emph{type
 erasure} however, that is to say that the compile-time type of values does not
persist at run-time. Primitive types are still implicitly tagged with their
type (which allows for untagged unions and predicates such as
@racket[number?]), but the type of a function cannot be determined at run-time
for example. This means that the type-variable-to-type mapping of @${ℰ} is not
effectively present at run-time with the current implementation of Typed
Racket.

@include-equation["envrt.rkt"]

@subsubsub*section{Evaluation contexts}

The operational semantics given below rely on the following evaluation
contexts:

@include-equation["Ectx.rkt"]

@; TODO: are other cases needed?

@subsubsub*section{Typing judgement}

@;{
 The type system of @typedracket relies on the following typing judgement. It
 indicates that the expression @${e} has type @${τ}. Additionally, if the
 run-time value of @${e} is @false-v then the propositions contained in @${φ⁻}
 are valid. If the run-time value of @${e} is not @false-v, then the
 propositions contained in @${φ⁺} are valid. Finally, @${e} is an alias for the
 @object @${o}. We use here the same terminology as
 @~cite["tobin-hochstadt_typed_2010"], which denotes by @object a sub-element
 of a variable (or a sub-element of the first argument of the function, when
 @R[τ @${φ⁺} @${φ⁻} @${o}] is the return type of a function).
}

@include-equation["GammaR.rkt" Γ]

@include-equation["GammaR.rkt" R]

The @Γ[⊢ e R] typing judgement indicates that the expression @${e} has type
@${τ}. The @${Γ} typing environment maps variables to their type (and to extra
information), while the @${Δ} environment stores the polymorphic type
variables, variadic polymorphic type variables and recursive type variables
which are in scope.

Additionally, the typing judgement indicates a set of propositions @${φ⁻}
which are known to be true when the run-time value of @${e} is @|false-v|, and
a set of propositions @${φ⁺} which are known to be true when the run-time
value of @${e} is @|true-v|@note{Any other value is treated in the same way as
 @|true-v|, as values other than @|false-v| are traditionally considered as
 true in language of the @lisp family.}. The propositions will indicate that the
value of a separate variable belongs (or does not belong) to a given type. For
example, the @${φ⁻} proposition @${@|Numberτ|_y} indicates that when @${e}
evaluates to @|false-v|, the variable @${y} necessarily holds an integer.

Finally, the typing judgement can indicate with @${o} that the expression @${
 e} is an alias for a sub-element of another variable in the environment. For
example, if the object @${o} is @${@carπ ∷ @cdrπ(y)}, it indicates that the
expression @${e} produces the same value that @racket[(car (cdr y))] would,
i.e. that it returns the second element of a (possibly improper) list stored
in @racket[y].

Readers familiar with abstract interpretation can compare the @${φ}
propositions to the Cartesian product of the abstract domains of pairs of
variables. A static analyser can track possible pairs of values contained in
pairs of distinct variables, and will represent this information using an
abstract domain which combinations of values may be possible, and which may
not. Occurrence typing similarly exploits the fact that the type of other
variables may depend on the value of @${τ}. @htodo{is this some weak form of
 dependent typing?}

@subsubsub*section{Types}

@Typedracket handles the types listed below. Aside from the top type (@${⊤})
which is the supertype of all other types, this list includes singleton types
for numbers, booleans, symbols and the @null-v value. The types @Numberτ and
@Symbolτ are the infinite unions of all number and symbol singletons,
respectively. Also present are function types (with fixed arguments,
homogeneous @emph{rest} arguments and the variadic polymorphic functions which
accept heterogeneous @emph{rest} arguments, as well as polymorphic
abstractions), unions of other types, intersections of other types, the type
of pairs and promises. The value assigned to a variadic polymorphic function's
rest argument will have a type of the form @List…τ[τ α]. Finally, @typedracket
allows recursive types to be described with the @recτ* combinator.

@include-equation["tausigma.rkt"]

Additionally, the @Booleanτ type is defined as the union of the @true-τ and
@false-τ singleton types.

@include-equation["tausigma.rkt" Boolean]

@subsubsub*section{Filters (value-dependent propositions)}

The filters associated with an expression are a set of positive (resp.
negative) propositions which are valid when the expression is true (resp.
false).

@include-equation["phi-psi-o-path.rkt" φ]

These propositions indicate that a specific subelement of a location has a
given type.

@include-equation["phi-psi-o-path.rkt" ψ]

The location can be a variable, or the special @${•} token, which denotes a
function's first parameter, when the propositions are associated with that
function's result. This allows us to express relations between the output of a
function and its input, without referring to the actual name of the parameter,
which is irrelevant. In other words, @${•} occurs in an α-normal form of a
function's type.

@include-equation["phi-psi-o-path.rkt" loc]

@Objects, which represent aliasing information, can either indicate that the
expression being considered is an alias for a sub-element of a variable, or
that no aliasing information is known.

@subsubsub*section{Objects (aliasing information)}

@include-equation["phi-psi-o-path.rkt" o]

Sub-elements are described via a chain of path elements which are used to
access the sub-element starting from the variable.

@subsubsub*section{Paths}

@include-equation["phi-psi-o-path.rkt" π]

The path concatenation operator @${∷} is associative. @htodo{Actually, we
 define it for pe∷π above, not for π∷π}. The @${@emptypath} is omitted from
paths with one or more elements, so we write @${car∷cdr} instead of @${
 car∷cdr∷@emptypath}.

@subsubsub*section{Path elements}

Path elements can be @carπ and @cdrπ, to indicate access to a pair's first or
second element, and @forceπ, to indicate that the proposition or object
targets the result obtained after forcing a promise. We will note here that
this obviously is only sound if forcing a promise always returns the same
result (otherwise the properties and object which held on a former value may
hold on the new result). Racket features promises which do not cache their
result. These could return a different result each time they are forced by
relying on external state. However, forcing a promise is generally assumed to
be an idempotent operation, and not respecting this implicit contract in
production code would be bad practice. Typed Racket disallows non-cached
promises altogether. We introduced a small module @racketmodname[delay-pure]
which allows the safe creation of non-cached promises.
@racketmodname[delay-pure] restricts the language to a small subset of
functions and operators which are known to not perform any mutation, and
prevents access to mutable variables. This ensures that the promises created
that way always produce the same value, without the need to actually cache
their result.

@include-equation["phi-psi-o-path.rkt" pe]

@subsubsub*section{Subtyping}
The subtyping judgement is @${@<:[τ δ]}. It indicates that @${τ} is a
subtype of @${σ} (or that @${τ} and @${σ} are the same type).

The @<:* relation is reflexive and transitive. When two or more types are all
subtypes of each other, they form an equivalence class. They are considered
different notations for the same type, and we note @=:[τ σ], whereas @≠:[τ σ]
indicates that @${τ} and @${σ} are not mutually subtypes of each other (but
one can be a strict subtype of the other).

@include-equation["subtyping.rkt" S-Reflexive]
@include-equation["subtyping.rkt" S-Transitive]

The @${⊥} type is a shorthand for the empty union @${(∪)}. It is a subtype of
every other type, and is not inhabited by any value. @textsc{S-Bot} can be
derived from @textsc{S-UnionSub}, by constructing an empty union.

@$p[@include-equation["subtyping.rkt" S-Top]
    @include-equation["subtyping.rkt" S-Bot]]

The singleton types @num-τ and @symτ[s] which are only inhabited by their
literal counterpart are subtypes of the more general @Numberτ or @Symbolτ
types, respectively.

@$p[@include-equation["subtyping.rkt" S-Number]
    @include-equation["subtyping.rkt" S-Symbol]]

The following subtyping rules are concerned with function types and
polymorphic types:

@$p[@include-equation["subtyping.rkt" S-Fun]
    @include-equation["subtyping.rkt" S-R]
    @include-equation["subtyping.rkt" S-Fun*]
    @include-equation["subtyping.rkt" S-Fun*-Fixed]
    @include-equation["subtyping.rkt" S-Fun*-Fixed*]
    @include-equation["subtyping.rkt" S-DFun]
    @include-equation["subtyping.rkt" S-Poly-α-Equiv]
    @include-equation["subtyping.rkt" S-PolyD-α-Equiv]
    @include-equation["subtyping.rkt" S-DFun-Fun*]]

@todo{@textsc{S-PolyD-α-Equiv} should use the substitution for a polydot
 (subst-dots?), not the usual subst.}

@todo{check the @textsc{S-DFun-Fun*} rule.}

@htodo{Try to detach the ∀ from the →, in case the → is nested further deep.
 If it works.}

The following rules are concerned with recursive types built with the
@racket[Rec] combinator. The @textsc{S-RecWrap} rule allows considering
@Numberτ a subtype of @recτ[r Numberτ] for example (i.e. applying the
recursive type combinator to a type which does not refer to @${r} is a no-op),
but it also allows deriving
@<:[@recτ[r @un[@consτ[τ r] @null-τ]] @un[@consτ[τ ⊤] @null-τ]]. The @textsc{
 S-RecElim} rule has the opposite effect, and is mainly useful to “upcast”
members of an union containing @${r}. It allows the deriving
@<:[@null-τ @recτ[r @un[@consτ[τ r] @null-τ]]]. The rules @textsc{S-RecStep}
and @textsc{S-RecUnStep} allow unraveling a single step of the recursion, or
assimilating an such an unraveled step as part of the recursive type.

@todo{TODO: renamings}

@$p[
 @include-equation["subtyping.rkt" S-RecWrap]
 @include-equation["subtyping.rkt" S-RecElim]
 @include-equation["subtyping.rkt" S-RecStep]
 @include-equation["subtyping.rkt" S-RecUnStep]]

The rules below describe how union and intersection types compare.

@$p[@include-equation["subtyping.rkt" S-UnionSuper]
    @include-equation["subtyping.rkt" S-UnionSub]
    @include-equation["subtyping.rkt" S-IntersectionSub]
    @include-equation["subtyping.rkt" S-IntersectionSuper]]

Finally, promises are handled by comparing the type that they produce when
forced, and pairs are compared pointwise. Dotted lists types, which usually
represent the type of the value assigned to a variadic polymorphic function's
“rest” argument

@$p[@include-equation["subtyping.rkt" S-Promise]
    @include-equation["subtyping.rkt" S-Pair]
    @include-equation["subtyping.rkt" S-DList]]

@subsubsub*section{Operational semantics}

@todo{TODO}

@subsubsub*section{Type validity rules}

Polymorphic type variables valid types if they are bound, that is if they are
present in the @${Δ} environment. Additionally variadic (i.e. dotted)
polymorphic type variables may be present in the environment. When this is the
case, they can be used as part of a @List…τ[τ α] type

@$p[@include-equation["te.rkt" TE-Var]
    @include-equation["te.rkt" TE-DList]]

@htodo{There are more rules needed (one for building every type, most are
 trivial).}

@htodo{isn't there any well-scopedness constraint for the φ?}

The following rules indicate that function types are valid if their use of
polymorphic type variables is well-scoped.

@$p[@include-equation["te.rkt" TE-DFun]
    @include-equation["te.rkt" TE-All]
    @include-equation["te.rkt" TE-DAll]
    @include-equation["te.rkt" TE-DPretype]]

The following rule indicates that types built using the recursive type
combinator @recτ* are valid if their use of the recursive type variable @${r}
is well-scoped.

@include-equation["te.rkt" TE-Rec]

The next rules are trivial, and state that the base types are valid, or simply
examine validity pointwise for unions, intersections, pairs, promises and
filters. @htodo{and objects}

@$p[
 @include-equation["te.rkt" TE-Trivial]
 @include-equation["te.rkt" TE-R]
 @include-equation["te.rkt" TE-Phi]
 @include-equation["te.rkt" TE-Psi]
 @include-equation["te.rkt" TE-Psi-Not]
 @include-equation["te.rkt" TE-Psi-Bot]
 ]

@subsubsub*section{Typing rules}

@todo{Add rule for the (optional?) simplification of intersections}

@$${
 \begin{aligned}
 \end{aligned}
}

@include-equation["trules.rkt" T-Promise]
@include-equation["trules.rkt" T-Symbol]
@include-equation["trules.rkt" T-Gensym]

@htodo{Are the hypotheses for T-Eq? necessary? After all, in Racket eq? works
 on Any.}

@include-equation["trules.rkt" T-Eq?]
@include-equation["trules.rkt" T-Var]
@include-equation["trules.rkt" T-Primop]
@include-equation["trules.rkt" T-True]
@include-equation["trules.rkt" T-False]
@include-equation["trules.rkt" T-Num]
@include-equation["trules.rkt" T-Null]

@htodo{The original TD-Map rule (p.95) seems wrong, as it allows un-dotted
 references to α in the function's type. But it is impossible to construct such
 a function, and the meaning of α in that case is unclear. I think the rule
 should instead expect a polymorphic function, with occurrences of α in τ_r
 replaced with the new β variable, as shown below.}

@include-equation["trules.rkt" T-DMap]

Below are the rules for the various flavours of lambda functions and
polymorphic abstractions.

@include-equation["trules.rkt" T-AbsPred]

@htodo{Technically, in the rules T-Abs and T-DAbs, we should keep any φ and o information concerning outer
 variables (those not declared within the lambda, and therefore still available
 after it finishes executing).}

@include-equation["trules.rkt" T-Abs]
@include-equation["trules.rkt" T-DAbs]

@todo{Should the φ⁺ φ⁻ o be preserved in T-TAbs and T-DTAbs?}

@include-equation["trules.rkt" T-TAbs]
@include-equation["trules.rkt" T-DTAbs]

The @${\vphantom{φ}@substφo[x ↦ z]} operation restricts the information
contained within a @${φ} or @${o} so that the result only contains information
about the variable @${x}, and renames it to @${z}. When applied to a filter
@${φ}, it corresponds to the @${\operatorname{abo}} and @${\operatorname{
  apo}} operators from
@~cite[#:precision "pp. 65,75" "tobin-hochstadt_typed_2010"].

The @${⊥} cases of the @${\operatorname{apo}} operator
from@~cite[#:precision "pp. 65,75" "tobin-hochstadt_typed_2010"] are covered
by the corresponding cases in the @${@restrict} and @${@remove} operators, and
therefore should not need to be included in our @${\vphantom{φ}@substφo[x ↦ z]}
operator.

@include-equation["trules.rkt" substφ]
@include-equation["trules.rkt" substo]

Below are the typing rules for the various flavours of function application and
instantiation of polymorphic abstractions.

@include-equation["trules.rkt" T-App]

@todo{For the inst rules, are the φ⁺ φ⁻ o preserved?}

@include-equation["trules.rkt" T-Inst]
@include-equation["trules.rkt" T-DInst]
@include-equation["trules.rkt" T-DInstD]
@include-equation["trules.rkt" T-If]

@htodo{The definition of Γ' does not specify what the other cases ≠ x are
 (they are the same as the original Γ, but this is only implicit).}

@include-equation["trules.rkt" Γ+]
@include-equation["trules.rkt" update]
@include-equation["trules.rkt" restrict]
@include-equation["trules.rkt" remove]

@;{Shouldn't no-overlap be simplified to @${@no-overlap(τ, τ') = (@<:[σ τ]
  ∧ @<:[σ τ′] ⇒ σ = ⊥)}? Then @${@restrict(τ,σ)} can be simplified to returning
 the most general type which is a subtype of τ and σ if one exists (or maybe
 simply returning the intersection of τ and σ).}

@todo{Δ is not available here.}
@todo{The non-nested use of σ is not quite correct syntactically speaking}

@include-equation["trules.rkt" no-overlap]

@htodo{Say that there are more rules in the implementation, to handle various
 boolean operations.}

@include-equation["trules.rkt" combinefilter]

@htodo{The Γ ⊢ x : τ … does not generate a Γ(x) = τ, I suspect. There should
 be indicated somewhere an equivalence between these two notations (and we
 should fix the @${Γ,x:update(…)}, as it is a third notation).}

@subsubsub*section{δ-rules}

@include-equation["deltarules.rkt"]
