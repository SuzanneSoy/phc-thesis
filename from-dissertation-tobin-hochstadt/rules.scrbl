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

@subsubsub*section{Notations}

We note a sequence of elements with @repeated{y}. When there is more than one
sequence involved in a rule or equation, we may use the notation
@repeated[#:n "n"]{y} to indicate that there are @${n} elements in the
sequence. Two sequences can be forced to have the same number of elements in
that way. We represent a set of elements (an “unordered” sequence) with the
notation @repeatset{y}. The use of ellipses in @polydotα{α} does not indicate
the repetition of @${α}. Instead, it indicates that @${α} is a @emph{
 variadic} polymorphic type variable: a placeholder for zero or more types
which will be substituted for occurrences of @${α} when the polymorphic type
is instantiated. These ellipses appear as such in the @typedracket source
code, and are the reason we use the notation @repeated{y} to indicate
repetition, instead of the ellipses commonly used for that purpose. FInally,
an empty sequence of repeated elements is sometimes noted @${ϵ}

The judgements are written following the usual convention, where @${Γ} is the
environment which associates variables to their type. The @${Δ} environment
contains the type variables within scope, and is mostly used to determine the
validity of types.

@$${@Γ[⊢ e R]}

The environments can be extended as follows:

@$${@Γ[@${x : τ} Δ @${\{α\}} ⊢ e R]}

The typing information @R associated with an expression contains the type of
the expression, as well as aliasing information and other propositions which
are known to be conditionally true depending on the value of the expression at
run-time. These pieces of information are described in more detail
@tech[#:key "R-typing-info"]{below}. Since the typing information @R is often
inlined in the typing judgement, a typing judgement will generally have the
following form:

@$${@Γ[⊢ e @R[τ φ⁺ φ⁻ o]]}

In this notation, the @${+} and @${-} signs in @${φ⁺} and @${φ⁻} are purely
syntactical, and serve to distinguish the positive and negative filters, which
are instances of the nonterminal @${φ}.

The various nonterminals used throughout the language are written in italics
and are defined using the notation:

@cases[@textit{nonterminal} #:first-sep "⩴"
       @acase{@textit{first case}}
       @acase{@textit{second case}}
       @acase{@textit{and so on}}]

Additionally, a symbol assigned to a nonterminal may be used as a placeholder
in rules and definitions, implicitly indicating that the element used to fill
in that placeholder should be an instance of the corresponding nonterminal.
When multiple such placeholders are present in a rule, they will be
subscripted to distinguish between the different occurrences. The subscripts
@${i}, @${j}, @${k} and @${l} are often used for repeated sequences.

In later chapters, we extend already-defined non-terminals using the notation:
 
@cases[@textit{nonterminal} #:first-sep "⩴"
       @acase{…}
       @acase{@textit{new case}}
       @acase{@textit{other new case}}]

Typing rules and other rules are described following the usual natural
deduction notation.

@$inferrule[@${@textit{hypothesis}\\ @textit{other hypothesis}}
            @${@textit{deduction}}
            @${@textsc{Rule-Name}}]

Metafunctions (i.e. functions which operate on types as syntactical elements,
or on other terms of the language) are written in a roman font. The meta-values
@|metatrue| and @|metafalse| indicate logical truth and falsehood respectively.

@$${\mathrm{metafunction}(x,y) = \begin{cases}
 @metatrue &@textif x = y + 1\\
 @metafalse &@otherwise
 \end{cases}}

Language operators are written in bold face:

@cases["e" #:first-sep "⩴"
       @acase{@num-e}
       @acase{…}]

Values are written using a bold italic font:

@cases["v" #:first-sep "⩴"
       @acase{@num-v}
       @acase{…}]

Type names start with a capital letter, and are written using a bold font:

@cases["τ,σ" #:first-sep "⩴"
       @acase{@num-τ}
       @acase{…}]

We indicate the syntactical substitution of @${y} with @${z} in @${w} using
the notation @${w@subst[y ↦ z]}. When given several elements to replace, the
substitution operator performs a parallel substitution (that is,
@${w@subst[x ↦ y y ↦ z]} will not replace the occurrences of @${y} introduced by
the first substitution).

@todo{Succinctly describe the other conventions used in the thesis, if any
 were omitted above.}

@todo{Define the meta substitution and equality operators precisely.}

@subsubsub*section{Names and bindings}

In the following sections, we assume that all type variable names which occur
in binding positions are unique. This assumption could be made irrelevant by
explicitly renaming in the rules below all type variables to fresh unique
ones. Performing this substitution would be a way of encoding a notion of
scope and the possibility for one identifier to hide another. However, the
Racket language features macros which routinely produce new binding forms. The
macro system in Racket is a hygienic one, and relies on a powerful model of
the notion of scope@~cite["flatt2016binding"]. Extending the rules below with
a simplistic model of Racket's notion of scope would not do justice to the
actual system, and would needlessly complicate the rules. Furthermore,
@typedracket only typechecks fully-expanded programs. In these programs, the
binding of each identifier has normally been determined@note.{@Typedracket
 actually still determines the binding for type variables by itself, but we
 consider this is an implementation detail.}

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

The @listv value notation is defined as a shorthand for a @|null-v|-terminated
linked list of pairs.

@include-equation["v.rkt" listv]

@;{
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
}

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

@deftech[#:key "R-typing-info"]{}
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
@false-τ singleton types, and the @Listτ type operator is a shorthand for
describing the type of @|null-v|-terminated heterogeneous linked lists of
pairs, with a fixed length. The @Listofτ type operator is a shorthand for
describing the type of @|null-v|-terminated homogeneous linked lists of pairs,
with an unspecified length.

@include-equation["tausigma.rkt" Boolean]
@include-equation["tausigma.rkt" Listτ]
@include-equation["tausigma.rkt" Listofτ]

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
The subtyping judgement is @${@<:[τ σ]}. It indicates that @${τ} is a
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

The semantics for the simplest expressions and for primitive functions are
expressed using δ-rules.

@$p[@include-equation["operational-semantics.rkt" E-Delta]
    @include-equation["operational-semantics.rkt" E-DeltaE]]

@include-equation["operational-semantics.rkt" δ-rules]

@include-equation["operational-semantics.rkt" δe-rules]

The @textsc{E-Context} rule indicates that when the expression has the shape
@${E[L]}, the subpart @${L} can be evaluated and replaced by its result. The
syntax @${E[L]} indicates the replacement of the only occurrence of @${[⋅]}
within an evaluation context @${E}. The evaluation context can then match an
expression with the same shape, thereby separating the @${L} part from its
context.

@include-equation["operational-semantics.rkt" E-Context]

The next rules handle β-reduction for the various flavours of functions.

@$p[@include-equation["operational-semantics.rkt" E-Beta]
    @include-equation["operational-semantics.rkt" E-Beta*]
    @include-equation["operational-semantics.rkt" E-BetaD]]

Instantiation of polymorphic abstractions is a no-op at run-time, because
@typedracket performs type erasure (no typing information subsists at
run-time, aside from the implicit tags used to distinguish the various
primitive data types: pairs, numbers, symbols, @null-v, @true-v, @false-v,
functions and promises).

@$p[@include-equation["operational-semantics.rkt" E-TBeta]
    @include-equation["operational-semantics.rkt" E-TDBeta]]

For simplicity, we assume that promises only contain pure expressions, and
therefore that the expression always produces the same value (modulo object
identity, i.e. pointer equality issues). In practice, @typedracket allows
expressions relying on external state, and caches the value obtained after
forcing the promise for the first time. The subset of the language which we
present here does not contain any mutable value, and does not allow mutation
of variables either, so the expression wrapped by promises is, by definition,
pure. We note here that we implemented a small library for @typedracket which
allows the creation of promises encapsulating a pure expression, and whose
result is not cached.

@include-equation["operational-semantics.rkt" E-Force]

Once the evaluation context rule has been applied to evaluate the condition of
an @ifop expression, the evaluation continues with the @emph{then} branch or
with the @emph{else} branch, depending on the condition's value. Following
@lisp tradition, all values other than @false-v are interpreted as a true
condition.

@$p[@include-equation["operational-semantics.rkt" E-If-False]
    @include-equation["operational-semantics.rkt" E-If-True]]

The @gensyme expression produces a fresh symbol @${@sym* ∈ 𝒮*}, which is
guaranteed to be different from all symbol literals @${s ∈ 𝒮}, and different
from all previous and future symbols returned by @|gensyme|. The @eq?op
operator can then be used to compare symbol literals and symbols produced by
@|gensyme|.

@$p[@include-equation["operational-semantics.rkt" E-Gensym]
    @include-equation["operational-semantics.rkt" E-Eq?-True]
    @include-equation["operational-semantics.rkt" E-Eq?-False]]

The semantics of @mapop are standard. We note here that @mapop can also be
used as a first-class function in @typedracket, and the same can be achieved
with the simplified semantics using the η-expansion
@;
@Λe[(α β) @λv[(@${x_f:@f→[(α) @R[β ϵ ϵ ∅]]}
                @${x_l:@Listofτ[α]}) @mapop[x_f x_l]]]
of the @mapop operator.

@$p[@include-equation["operational-semantics.rkt" E-Map-Pair]
    @include-equation["operational-semantics.rkt" E-Map-Null]]

@subsubsub*section{Type validity rules}

Polymorphic type variables valid types if they are bound, that is if they are
present in the @${Δ} environment. Additionally variadic (i.e. dotted)
polymorphic type variables may be present in the environment. When this is the
case, they can be used as part of a @List…τ[τ α] type.

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
 @include-equation["te.rkt" TE-Psi-Bot]]

@subsubsub*section{Typing rules}

The rules below relate the simple expressions to their type.

@htodo{Are the hypotheses for T-Eq? necessary? After all, in Racket eq? works
 on Any.}

@$p[@include-equation["trules.rkt" T-Symbol]
    @include-equation["trules.rkt" T-Gensym]
    @include-equation["trules.rkt" T-Promise]
    @include-equation["trules.rkt" T-Var]
    @include-equation["trules.rkt" T-Primop]
    @include-equation["trules.rkt" T-True]
    @include-equation["trules.rkt" T-False]
    @include-equation["trules.rkt" T-Num]
    @include-equation["trules.rkt" T-Null]
    @include-equation["trules.rkt" T-Eq?]]

Below are the rules for the various flavours of lambda functions and
polymorphic abstractions.

@htodo{Technically, in the rules T-Abs and T-DAbs, we should keep any φ and o
 information concerning outer variables (those not declared within the lambda,
 and therefore still available after it finishes executing).}

@todo{Should the φ⁺ φ⁻ o be preserved in T-TAbs and T-DTAbs?}

@$p[@include-equation["trules.rkt" T-AbsPred]
    @include-equation["trules.rkt" T-Abs]
    @include-equation["trules.rkt" T-Abs*]
    @include-equation["trules.rkt" T-DAbs]
    @include-equation["trules.rkt" T-TAbs]
    @include-equation["trules.rkt" T-DTAbs]]

The @${\vphantom{φ}@substφo[x ↦ z]} operation restricts the information
contained within a @${φ} or @${o} so that the result only contains information
about the variable @${x}, and renames it to @${z}. When applied to a filter
@${φ}, it corresponds to the @${\operatorname{abo}} and @${\operatorname{
  apo}} operators from
@~cite[#:precision "pp. 65,75" "tobin-hochstadt_typed_2010"].

The @${⊥} cases of the @${\operatorname{apo}} operator
from@~cite[#:precision "pp. 65,75" "tobin-hochstadt_typed_2010"] are covered
by the corresponding cases in the @${@restrict} and @${@remove} operators
defined below, and therefore should not need to be included in our @${
 \vphantom{φ}@substφo[x ↦ z]} operator. The subst

@include-equation["trules.rkt" substφ]
@include-equation["trules.rkt" substo]

@htodo{The definition of Γ' does not specify what the other cases ≠ x are
 (they are the same as the original Γ, but this is only implicit).}

The @racket[map] function can be called like any other function, but also has
a specific rule allowing a more precise result type when mapping a polymorphic
function over a @List…τ[τ α]. @racket[ormap], @racket[andmap] and
@racket[apply] similarly have special rules for variadic polymorphic lists. We
include the rule for @racket[map] below as an example. The other rules are
present in @~cite[#:precision "pp. 96" "tobin-hochstadt_typed_2010"].

@htodo{The original TD-Map rule (p.95) seems wrong, as it allows un-dotted
 references to α in the function's type. But it is impossible to construct such
 a function, and the meaning of α in that case is unclear. I think the rule
 should instead expect a polymorphic function, with occurrences of α in τ_r
 replaced with the new β variable, as shown below.}

@include-equation["trules.rkt" T-DMap]

Below are the typing rules for the various flavours of function application and
instantiation of polymorphic abstractions.

@todo{For the inst rules, are the φ⁺ φ⁻ o preserved?}

@$p[@include-equation["trules.rkt" T-App]
    @include-equation["trules.rkt" T-Inst]
    @include-equation["trules.rkt" T-DInst]
    @include-equation["trules.rkt" T-DInstD]]

@;=====================TODO:write something vvvvvvvvvvvv

The rule for @ifop uses the information gained in the condition to narrow the
type of variables in the @emph{then} and @emph{else} branches. This is the
core rule of the occurrence typing aspect of @|typedracket|.

@include-equation["trules.rkt" T-If]

The @${Γ + φ} operator narrows the type of variables in the environment using
the information contained within @${φ}.

@include-equation["trules.rkt" Γ+]

The @update operator propagates the information contained within a @${ψ} down
to the affected part of the type.

@include-equation["trules.rkt" update]

The @update operator can then apply @restrict to perform the intersection of
the type indicated by the @|ifop|'s condition and the currently-known type for
the subpart of the considered variable.

@todo{How do @restrict and @remove behave on intersections?}

@include-equation["trules.rkt" restrict]

The @update operator can also apply
the @remove operator when the condition determined that the subpart of the
considered variable is @emph{not} of a given type.

@include-equation["trules.rkt" remove]

@;{Shouldn't no-overlap be simplified to @${@no-overlap(τ, τ') = (@<:[σ τ]
  ∧ @<:[σ τ′] ⇒ σ = ⊥)}? Then @${@restrict(τ,σ)} can be simplified to returning
 the most general type which is a subtype of τ and σ if one exists (or maybe
 simply returning the intersection of τ and σ).}

@todo{Δ is not available here.}
@todo{The non-nested use of σ is not quite correct syntactically speaking}

The @restrict operator and the @simplify* operator described later both rely
on @no-overlap to determine whether two types have no intersection, aside from
the @${⊥} type.

@include-equation["trules.rkt" no-overlap]

@htodo{Say that there are more rules in the implementation, to handle various
 boolean operations.}

The @combinefilter operator is used to combine the @${φ} information from the
two branches of the @ifop expression, based on the @${φ} information of the
condition. This allows @typedracket to correctly interpret @racket[and],
@racket[or] as well as other boolean operations, which are implemented as
macros translating down to nested @racket[if] conditionals. @Typedracket will
therefore be able to determine that in the @emph{then} branch of
@racket[(if (and (string? x) (number? y)) 'then 'else)], the type of
@racketid[x] is @racket[String], and the type of @racketid[y] is
@racket[Number]. We only detail a few cases of combinefilter here, a more
complete description of the operator can be found
in@~cite[#:precision "pp. 69,75–84" "tobin-hochstadt_typed_2010"].

@include-equation["trules.rkt" combinefilter]

@htodo{The Γ ⊢ x : τ … does not generate a Γ(x) = τ, I suspect. There should
 be indicated somewhere an equivalence between these two notations (and we
 should fix the @${Γ,x:update(…)}, as it is a third notation).}

@subsubsub*section{Simplification of intersections}

In some cases, intersections are simplified, and the eventual resulting @${⊥}
types propagate outwards through pairs (and structs, which we do not model
here). The @simplify* and @propagate⊥ operators show how these simplification
and propagation steps are performed. The simplification step mostly consists
in distributing intersections over unions and pairs, and collapsing pairs and
unions which contain @${⊥}, for the traversed parts of the type.

@include-equation["simplify.rkt" Simplify1]

@${@simplify[τ]} is applied pointwise in other cases:

@include-equation["simplify.rkt" Simplify2]

@include-equation["simplify.rkt" Propagate⊥]

@todo{Apply the intersections on substituted poly types after an inst (or rely
 on the sutyping rule for intersections to recognise that ⊥ is a subtype of the
 resulting type?)}.

@subsubsub*section{δ-rules}

Finally, the type and semantics of primitive functions are expressed using the
δ-rules given below.

@include-equation["deltarules.rkt"]

@subsubsub*section{Soundness proof}

Since @typedracket is an existing language which we use for our
implementation, and not a new language, we do not provide here a full proof of
correctness.

We invite instead the interested reader to refer to the proof sketches given
in@~cite[#:precision "pp. 68–84" "tobin-hochstadt_typed_2010"]. These proof
sketches only cover a subset of the language presented here, namely a language
with variables, function applictation, functions of a single argument, pairs,
booleans, numbers and @ifop conditionals with support for occurrence typing.
Since occurrence typing is the main focus of the proof, the other extensions
aggregated here should not significantly threaten its validity.