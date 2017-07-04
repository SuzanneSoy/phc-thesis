#lang scribble/manual

@require["util.rkt"
         (for-label (only-meta-in 0 typed/racket)
                    typed/racket/class)
         scribble/example
         racket/string]
@(use-mathjax)

@(define tr-eval (make-eval-factory '(typed/racket)))

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)
       #:tag "tr-chap"]{@|Typedracket|}

We start this section with some history: Lisp, @emph{the} language with lots
of parentheses, shortly following Fortran as one of the first high-level
programming languages, was initially designed between 1956 and 1958, and
subsequently implemented@~cite["McCarthyHistoryLisp"]. Dialects of Lisp
generally support a variety of programming paradigms, including (but not
limited to) functional programming and object-oriented programming (e.g. via
CLOS, the Common Lisp Object System). One of the the most proeminent aspects
of Lisp is homoiconicity, the fact that programs and data structures look the
same. This enables programs to easily manipulate other programs, and led to
the extensive use of macros. Uses of macros usually look like function
applications, but, instead of invoking a target function at run-time, a macro
will perform some computation at compile-time, and expand to some new code,
which is injected as a replacement of the macro's use.

The two main dialects of Lisp are Common Lisp and Scheme. Scheme follows a
minimalist philosophy, where a small core is
standardised@~cite["r5rs" "r6rs" "r7rs"] and subsequently extended via macros
and additional function definitions.

Racket, formerly named PLT Scheme, started as a Scheme implementation. Racket
evolved, and the Racket Manifesto@~cite["racketmanifesto"] presents it as a
``programming-language programming language'', a language which helps with the
creation of small linguistic extensions as well as entirely new languages. The
Racket ecosystem features many languages covering many paradigms:

@itemlist[
 @item{The @racketmodname[racket/base] language is a full-featured programming
  language which mostly encourages functional programming.}
 @item{@racketmodname[racket/class] implements
  @seclink["classes" #:doc '(lib "scribblings/guide/guide.scrbl")]{an
   object-oriented system}, implemented atop @racketmodname[racket/base] using
  macros, and can be used along with the rest of the @racketmodname[racket/base]
  language.}
 @item{@racketmodname[racklog] is a logic programming language in the style of
  prolog. The Racket ecosystem also includes an implementation of
  @racketmodname[datalog].}
 @item{@seclink["top" #:doc '(lib "scribblings/scribble/scribble.scrbl")]{
   Scribble} can be seen as an alternative to @|LaTeX|, and is used to create
  the @seclink["top" #:doc '(lib "scribblings/main/start.scrbl")]{Racket
   documentation}. It also supports literate programming, by embedding chunks of
  code in the document which are then aggregated together. This thesis is
  in fact written using Scribble.}
 @item{@racketmodname[slideshow] is a @deftech{DSL} (domain-specific language)
  for the creation of presentations, and can be thought as an alternative to
  Beamer and SliTeX.}
 @item{@racketmodname[r5rs] and @racketmodname[r6rs] are implementations of
  the corresponding scheme standards.}
 @item{@seclink["top" #:doc '(lib "redex/redex.scrbl")]{Redex} is a
  @usetech{DSL} which allows the specification of reduction
  semantics for programming languages. It features tools to explore and test
  the defined semantics.}
 @item{@|Typedracket|@~cite["tobin-hochstadt_design_2008"
                            "tobin-hochstadt_typed_2010"] is a typed variant of
  the main @racketmodname[racket] language. It is implemented as a macro which
  takes over the whole body of the program. That macro fully expands all other
  macros in the program, and then typechecks the expanded program.}
 @item{@seclink["top" #:doc '(lib "turnstile/scribblings/turnstile.scrbl")]{
   @|Turnstile|} allows the creation of new typed languages. It takes a
  different approach when compared to @|typedracket|, and threads the type
  information through assignments and special forms, in order to be able to
  typecheck the program during expansion, instead of doing so afterwards.}]

In the remainder of this section, we will present the features of
@|typedracket|'s type system, and then present formal semantics for a subset
of those, namely the part which is relevant to our work.
@other-doc['(lib "typed-racket/scribblings/ts-guide.scrbl")] and
@other-doc['(lib "typed-racket/scribblings/ts-reference.scrbl")] provide good
documentation for programmers who desire to use @|typedracket|; we will
therefore keep our overview succinct and gloss over most details.

@asection{
 @atitle{Overview of Typed Racket's type system}

 @asection{
  @atitle{Simple primitive types}
   
  @Typedracket has types matching Racket's baggage of primitive values:
  @racket[Number], @racket[Boolean], @racket[Char], @racket[String],
  @racket[Void]@note{The @racket[Void] type contains only a single value,
   @racket[#,(void)], and is equivalent to the @racketid[void] type in
   @|C-language|. It is the equivalent of @racketid[unit] of @CAML and
   @|haskell|, and is often used as the return type of functions which perform
   side-effects. It should not be confused with @racket[Nothing], the bottom
   type which is not inhabited by any value, and is similar to the type of
   @|haskell|'s @racketid[undefined]. @racket[Nothing] can be used for example
   as the type of functions which never return — in that way it is similar to
   @|C-language|'s @tt["__attribute__ ((__noreturn__))"].} and so on.

  @examples[#:label #f #:eval (tr-eval)
            (ann #true Boolean)
            243
            "Hello world"
            #\c
            (code:comment "The void function produces the void value")
            (code:comment "Void values on their own are not printed,")
            (code:comment "so we place it in a list to make it visible.")
            (list (void))]
  
  For numbers, @|typedracket| offers a ``numeric tower'' of
  partially-overlapping types: @racket[Positive-Integer] is a subtype of
  @racket[Integer], which is itself a subtype of @racket[Number]. @racket[Zero],
  the type containing only the number 0, is a both a subtype of
  @racket[Nonnegative-Integer] (numbers ≥ 0) and of @racket[Nonpositive-Integer]
  (numbers ≤ 0).

  @|Typedracket| also includes a singleton type for each primitive value of
  these types: we already mentioned @racket[Zero], which is an alias of the
  @racket[0] type. Every number, character, string and boolean value can be used
  as a type, which is only inhabited by the same number, character, string or
  boolean value. For example, @racket[243] belongs to the singleton type
  @racket[243], which is a subtype of @racket[Positive-Integer].

  @examples[#:label #f #:eval (tr-eval)
            0
            (ann 243 243)
            #t]}

 @asection{
  @atitle{Pairs and lists}

  Pairs are the central data structure of most Lisp dialects. They are used to
  build linked lists of pairs, terminated by @racket['()], the null element. The
  null element has the type @racket[Null], while the pairs which build the list
  have the type @racket[(Pairof _A _B)], where @racketid[_A] and @racketid[_B]
  are replaced by the actual types for the first and second elements of the
  pair. For example, the pair built using @racket[(cons 729 #true)], which
  contains @racket[729] as its first element, and @racket[#true] as its second
  element, has the type @racket[(Pairof Number Boolean)], or using the most
  precise singleton types, @racket[(Pairof 729 #true)].

  @examples[#:label #f #:eval (tr-eval)
            (cons 729 #true)
            '(729 . #true)]

  Heterogeneous linked lists of fixed length can be given a precise type by
  nesting the same number of pairs at the type level. For example, the list
  built with @racket[(list 81 #true 'hello)] has the type
  @racket[(List Number Boolean Symbol)], which is a shorthand for the type
  @racket[(Pairof Number (Pairof Boolean (Pairof Symbol Null)))]. Lists in
  @|typedracket| can thus be seen as the equivalent of a chain of nested
  2-tuples in languages like @|CAML| or @|haskell|. The analog in
  object-oriented languages with support for generics would be a class
  @tt["Pair<A, B>"], where the generic type argument @racketid[B] could be
  instantiated by another instance of @tt["Pair"], and so on.

  @examples[#:label #f #:eval (tr-eval)
            (cons 81 (cons #true (cons 'hello null)))
            (ann (list 81 #true 'hello)
                 (Pairof Number (Pairof Boolean (Pairof Symbol Null))))]

  The type of variable-length homogeneous linked lists can be described using
  the @racket[Listof] type operator. The type @racket[(Listof Integer)] is
  equivalent to @racket[(Rec R (U (Pairof Integer R) Null))]. The @racket[Rec]
  type operator describes @seclink["tr-presentation-recursive-types"]{recursive
   types}, and @racket[U] describes @seclink["tr-presentation-unions"]{unions}.
  Both of these features are described below, for now we will simply say that
  the previously given type is a recursive type @racket[R], which can be a
  @racket[(Pairof Integer R)] or @racket[Null] (to terminate the linked list).

  @examples[#:label #f #:eval (tr-eval)
            (ann (range 0 5) (Listof Number))]}

 @asection{
  @atitle{Symbols}

  Another of Racket's primitive datatypes is symbols. Symbols are interned
  strings: two occurrences of a symbol produce values which are pointer-equal if
  the symbols are equal (i.e. they represent the same string)@note{This is true
   with the exception of symbols created with @racket[gensym] and the like.
   @racket[gensym] produces a fresh symbol which is not interned, and therefore
   different from all existing symbols, and different from all symbols created
   in the future.}.

  @|Typedracket| includes the @racket[Symbol] type, to which all symbols
  belong. Additionally, there is a singleton type for each symbol: the type
  @racket['foo] is only inhabited by the symbol @racket['foo].

  @examples[#:label #f #:eval (tr-eval)
            'foo]

  Singleton types containing symbols can be seen as similar to constructors
  without arguments in @|CAML| and @|haskell|, and as globally unique enum
  values in object-oriented languages. The main difference resides in the scope
  of the declaration: two constructor declarations with identical names in two
  separate files will usually give distinct types and values. Similarly, when
  using the ``type-safe enum'' design pattern, two otherwise identical
  declarations of an enum will yield objects of different types. In contrast,
  two uses of an interned symbols in Racket and @|typedracket| will produce
  identical values and types. A way of seeing this is that symbols are similar
  to constructors (in the functional programming sense) or enums which are
  implicitly declared globally.

  @examples[#:label #f #:eval (tr-eval)
            (module m1 typed/racket
              (define sym1 'foo)
              (provide sym1))
            (module m2 typed/racket
              (define sym2 'foo)
              (provide sym2))
            (require 'm1 'm2)
            (code:comment "The tow independent uses of 'foo are identical:")
            (eq? sym1 sym2)]
 }
  
 @asection{
  @atitle[#:tag "tr-presentation-unions"]{Unions}

  These singleton types may not seem very useful on their own. They can however
  be combined together with union types, which are built using the @racket[U]
  type operator.
  
  The union type @racket[(U 0 1 2)] is inhabited by the values @racket[0],
  @racket[1] and @racket[2], and by no other value. The @racket[Boolean] type is
  actually defined as @racket[(U #true #false)], i.e. the union of the singleton
  types containing the @racket[#true] and @racket[#false] values, respectively.
  The @racket[Nothing] type, which is not inhabited by any value, is defined as
  the empty union @racket[(U)]. The type @racket[Any] is the top type, i.e. it
  is a super-type of all other types, and can be seen as a large union including
  all other types, including those which will be declared later or in other
  units of code.

  Unions of symbols are similar to variants which contain zero-argument
  constructors, in @|CAML| or @|haskell|.

  @examples[#:label #f #:eval (tr-eval)
            (define v : (U 'foo 'bar) 'foo)
            v
            (set! v 'bar)
            v
            (code:comment "This throws an error at compile-time:")
            (eval:error (set! v 'oops))]

  A union such as @racket[(U 'ca (List 'cb Number) (List 'cc String Symbol))]
  can be seen as roughly the equivalent of a variant with three constructors,
  @racketid[ca], @racket[cb] and @racketid[cc], where the first has no
  arguments, the second has one argument (a @racket[Number]), and the third has
  two arguments (a @racket[String] and a @racket[Symbol]).

  The main difference is that a symbol can be used as parts of several unions,
  e.g. @racket[(U 'a 'b)] and @racket[(U 'b 'c)], while constructors can often
  only be part of the variant used to declare them. Unions of symbols are in
  this sense closer to @|CAML|'s so-called polymorphic
  variants@~cite["minskyRealWorldOCaml"] than to regular variants.
  
  @examples[#:label #f #:eval (tr-eval)
            (define-type my-variant (U 'ca
                                       (List 'cb Number)
                                       (List 'cc String Symbol)))
            (define v₁ : my-variant 'ca)
            (define v₂ : my-variant (list 'cb 2187))
            (define v3 : my-variant (list 'cc "Hello" 'world))]

  Finally, it is possible to mix different sorts of types within the same
  union: the type @racket[(U 0 #true 'other)] is inhabited by the number
  @racket[0], the boolean @racket[#true], and the symbol @racket['other].
  Translating such an union to a language like @|CAML| could be done by
  explicitly tagging each case of the union with a distinct constructor.

  Implementation-wise, all values in the so-called ``untyped'' version of
  Racket are tagged: a few bits within the value's representation are reserved
  and used to encode the value's type. When considering the target of a pointer
  in memory, Racket is therefore able to determine if the pointed-to value is a
  number, boolean, string, symbol and so on. Typed Racket preserves these
  run-time tags. They can then be used to detect the concrete type of a value
  when its static type is a union. This detection is done simply by using
  Racket's predicates: @racket[number?], @racket[string?], @racket[symbol?]
  etc.}

 @asection{
  @atitle{Intersections}

  Intersections are the converse of unions: instead of allowing a mixture of
  values of different types, an intersection type, described using the
  @racket[∩] type operator, only allows values which belong to all types.

  The intersection type @racket[(∩ Nonnegative-Integer Nonpositive-Integer)] is
  the singleton type @racket[0]. The intersection of @racket[(U 'a 'b 'c)] and
  @racket[(U 'b 'c 'd)] will be @racket[(U 'b 'c)], as @racket['b] and
  @racket['c] belong to both unions.

  @examples[
 #:label #f #:eval (tr-eval)
 (code:comment ":type shows the given type, or a simplified version of it")
 (:type (∩ (U 'a 'b 'c) (U 'b 'c 'd)))]
  
  @|Typedracket| is able to reduce some intersections such as those given above
  at compile-time. However, in some cases, it is forced to keep the intersection
  type as-is. For example, structs (@seclink["tr-presentation-structs"]{
   describled below} can, using special properties, impersonate functions. This
  mechanism is similar to PHP's @tt["__invoke"], the ability to overload
  @tt["operator()"] in @|CPP|. @|Typedracket| does not handle these properties
  (yet), and therefore cannot determine whether a given struct type also
  impersonates a function or not. This means that the intersection
  @racket[(∩ s (→ Number String))], where @racket[s] is a struct type, cannot be
  reduced to @racket[Nothing], because @|typedracket| cannot determine whether
  the struct @racket[s] can act as a function or not.

  Another situation where @|typedracket| cannot reduce the intersection is when
  intersecting two function types (@seclink["tr-presentation-functions"]{
   presented below}).

  @racketblock[
 (∩ (→ Number String) (→ Number Symbol))
 (∩ (→ Number String) (→ Boolean String))]

  The first intersection seems like could be simplified to
  @racket[(→ Number String) (→ Number Symbol)], and the second one could be
  simplified to @racket[(→ (U Number Boolean) String)], however the equivalence
  between these types has not been implemented (yet) in @|typedracket|, so we do
  not rely on them. Note that this issue is not a soundness issue: it only
  prevents passing values  types to which they belong in principle, but it
  cannot be exploited to assign a value to a variable with an incompatible type.

  Finally, when some types are intersected with a polymorphic type variable,
  the intersection cannot be computed until the polymorphic type is
  instantiated.
  
  When @|typedracket| is able to perform a simplification, occurrences of
  @racket[Nothing] (the bottom type) propagate outwards in some cases, pairs and
  struct types which contain @racket[Nothing] as one of their elements being
  collapsed to @racket[Nothing]. This propagation of @racket[Nothing] starts
  from occurrences of @racket[Nothing] in the parts of the resulting type which
  are traversed by the intersection operator. It collapses the containing pairs
  and struct types to @racket[Nothing], moving outwards until the @racket[∩]
  operator itself is reached. In principle, the propagation could go on past
  that point, but this is not implemented yet in @|typedracket|@note{See
   @hyperlink["https://github.com/racket/typed-racket/issues/552"]{Issue #552}
   on @|typedracket|'s GitHub repository for more details on what prevents
   implementing a more aggressive propagation of @racket[Nothing].}.

  The type @racket[(∩ 'a 'b)] therefore gets simplified to @racket[Nothing],
  and the type @racket[(∩ (Pairof 'a 'x) (Pairof 'b 'x))] also simplifies to
  @racket[Nothing] (@|typedracket| initially pushes the intersection down the
  pairs, so that the type first becomes @racket[(Pairof (∩ 'a 'b) (∩ 'x 'x))],
  which is simplified to @racket[(Pairof Nothing 'x)], and the occurrence of
  @racket[Nothing] propagates outwards). However, if the user directly specifies
  the type @racket[(Pairof (∩ 'a 'b) Integer)], it is simplified to
  @racket[(Pairof Nothing Integer)], but the @racket[Nothing] does not propagate
  outwards beyond the initial use of @racket[∩].

  @examples[#:label #f #:eval (tr-eval)
            (:type (∩ 'a 'b))
            (:type (∩ (Pairof 'a 'x) (Pairof 'b 'x)))
            (:type (Pairof (∩ 'a 'b) Integer))]

  A simple workaround exists: the outer type, which could be collapsed to
  @racket[Nothing], can be intersected again with a type of the same shape. The
  outer intersection will traverse both types (the desired one and the
  ``shape''), and propagate the leftover @racket[Nothing] further out.

  @examples[#:label #f #:eval (tr-eval)
            (:type (Pairof (∩ 'a 'b) Integer))
            (:type (∩ (Pairof (∩ 'a 'b) Integer)
                      (Pairof Any Any)))]

  These intersections are not very interesting on their own, as in most cases
  it is possible to express the resulting simplified type without using the
  intersection operator. They become more useful when mixed with polymorphic
  types: intersecting a polymorphic type variable with another type can be used
  to restrict the actual values that may be used. The type @racket[(∩ A T)],
  where @racket[A] is a polymorphic type variable and @racket[T] is a type
  defined elsewhere, is equivalent to the use of bounded type parameters in
  @|java| or @|csharp|. In @|csharp|, for example, the type @racket[(∩ A T)]
  would be written using an @tt["where A : T"] clause.}
 
 @asection{
  @atitle[#:tag "tr-presentation-structs"]{Structs}

  Racket also supports @racket[struct]s, which are mappings from fields to
  values. A struct is further distinguished by its struct type: instances of two
  struct types with the same name and fields, declared in separate files, can be
  differentiated using the predicates associated with these structs. Structs in
  Racket can be seen as the analog of classes containing only fields (but no
  methods) in @csharp or @|java|. Such classes are sometimes called ``Plain Old
  Data (POD) Objects''. Structs belong to a single-inheritance hierarchy:
  instances of the descendents of a struct type are recognised by their
  ancestor's predicate. When a struct inherits from another, it includes its
  parent's fields, and can add extra fields of its own.

  Each struct declaration within a @|typedracket| program additionally declares
  corresponding type.

  @examples[#:label #f #:eval (tr-eval)
            (struct parent ([field₁ : (Pairof String Symbol)])
              #:transparent)
            (struct s parent ([field₂ : Integer]
                              [field₃ : Symbol])
              #:transparent)
            (s (cons "x" 'y) 123 'z)]

  In @|typedracket|, structs can have polymorphic type arguments, which can be
  used inside the types of the struct's fields.

  @examples[#:label #f #:eval (tr-eval)
            (struct (A B) poly-s ([field₁ : (Pairof A B)]
                                  [field₂ : Integer]
                                  [field₃ : B])
              #:transparent)
            (poly-s (cons "x" 'y) 123 'z)]

  Racket further supports
  @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{struct type
   properties}, which can be seen as a limited form of method definitions for a
  struct, thereby making them closer to real objects. The same struct type
  property can be implemented by many structs, and the declaration of a struct
  type property is therefore roughly equivalent to the declaration of an
  interface with a single method.

  Struct type properties are often considered a low-level mechanism in Racket.
  Among other things, a struct type property can only be used to define a single
  property at a time. When multiple ``methods'' have to be defined at once (for
  example, when defining the @racket[prop:equal+hash] property, which requires
  the definition of an equality comparison function, and two hashing functions),
  these can be grouped together in a list of functions, which is then used as
  the property's value.
  ``@seclink["struct-generics"
             #:doc '(lib "scribblings/reference/reference.scrbl")]{
   Generic interfaces}'' are a higher-level feature, which among other things
  allow the definition of multiple ``methods'' as part of a single generic
  interface, and offers a friendlier API for specifying the ``generic
  interface'' itself (i.e. what Object Oriented languages call an interfece), as
  and for specifying the implementation of said interface.

  @|Typedracket| unfortunately offers no support for struct type properties and
  generic interfaces for now. It is impossible to assert that a struct
  implements a given property at the type level, and it is also for example not
  possible to describe the type of a function accepting any struct implementing
  a given property or generic interface. Finally, no type checks are performed
  on the body of functions bound to such properties, and to check verifies that
  a function implementation with the right signature is supplied to a given
  property. Since struct type properties and generics cannot be used in a
  type-safe way for now, we refrain from using these features, and only use them
  to implement some very common properties@note{We built a thin macro wrapper
   which allows typechecking the implementation and signature of the functions
   bound to these two properties.}: @racket[prop:custom-write] which is the
  equivalent of @|java|'s @tt["void toString()"], and @racket[prop:equal+hash]
  which is equivalent to @|java|'s @tt["boolean equals(Object o)"] and
  @tt["int hashCode()"].

 }
 
 @asection{
  @atitle[#:tag "tr-presentation-functions"]{Functions}
   
  @|Typedracket| provides rich function types, to support some of the flexible
  use patterns allowed by Racket.

  The simple function type below indicates that the function expects two
  arguments (an integer and a string), and returns a boolean:

  @racketblock[(→ Integer String Boolean)]

  We note that unlike @|haskell| and @|CAML| functions, Racket functions are
  not implicitly curried. To express the corresponding curried function type,
  one would write:

  @racketblock[(→ Integer (→ String Boolean))]

  A function may additionally accept optional positional arguments, and keyword
  (i.e. named) arguments, both mandatory and optional:

  @racketblock[
 (code:comment "Mandatory string, optional integer and boolean arguments:")
 (->* (String) (Integer Boolean) Boolean)
 (code:comment "Mandatory keyword arguments:")
 (→ #:size Integer #:str String Boolean)
 (code:comment "Mandatory #:str, optional #:size and #:opt:")
 (->* (#:str String) (#:size Integer #:opt Boolean) Boolean)]

  Furthermore, functions in Racket accept a catch-all ``rest'' argument, which
  allows for the definition of variadic functions. Typed racket also allows
  expressing this at the type level, as long as the arguments covered by the
  ``rest'' clause all have the same type:

  @racketblock[
 (code:comment "The function accepts one integer and any number of strings:")
 (-> Integer String * Boolean)
 (code:comment "Same thing with an optional symbol inbetween: ")
 (->* (Integer) (Symbol) #:rest String Boolean)]

  One of @|typedracket|'s main goals is to be able to typecheck idiomatic
  Racket programs. Such programs may include functions whose return type depends
  on the values of the input arguments. Similarly, @racket[case-lambda] can be
  used to create lambda functions which dispatch to multiple behaviours based on
  the number of arguments passed to the function.

  @|Typedracket| provides the @racket[case→] type operator, which can be used to
  describe the type of these functions:

  @racketblock[
 (code:comment "Allows 1 or 3 arguments, with the same return type.")
 (case→ (→ Integer Boolean)
        (→ Integer String Symbol Boolean))
 (code:comment "A similar type based on optional arguments allows 1, 2 or 3")
 (code:comment " arguments in contrast:")
 (->* (Integer) (String Symbol) Boolean)
 (code:comment "The output type can depend on the input type:")
 (case→ (→ Integer Boolean)
        (→ String Symbol))
 (code:comment "Both features (arity and dependent output type) can be mixed")
 (case→ (→ Integer Boolean)
        (→ Integer String (Listof Boolean)))]

  Another important feature, which can be found in the type system of most
  functional programming languages, and most object-oriented languages, is
  parametric polymorphism. @|Typedracket| allows the definition of polymorphic
  structs, as detailed above, as well as polymorphic functions. For example, the
  function @racket[cons] can be considered as a polymorphic function with two
  polymorphic type arguments @racket[A] and @racket[B], which takes an argument
  of type @racket[A], an argument of type @racket[B], and returns a pair of
  @racket[A] and @racket[B].

  @racketblock[(∀ (A B) (→ A B (Pairof A B)))]

  @|Typedracket| supports polymorphic functions with multiple polymorphic type
  variables, as the one shown above. Furthermore, it allows one of the
  polymorphic variables to be followed by ellipses, indicating a variable-arity
  polymorphic type@~cite["tobin-hochstadt_typed_2010"]. The dotted polymorphic
  type variable can be instantiated with a tuple of types, and will be replaced
  with that tuple where it appears. For example, the type

  @racketblock[(∀ (A B ...) (→ (List A A B ...) Void))]

  can be instantiated with @racket[Number String Boolean], which would yield
  the type for a function accepting a list of four elements: two numbers, a
  string and a boolean.

  @racketblock[(→ (List Number Number String Boolean) Void)]

  Dotted polymorphic type variables can only appear in some places. A dotted
  type variable can be used as the tail of a @racket[List] type, so
  @racket[(List Number B ...)] (a @racket[String] followed by any number of
  @racket[B]s) is a valid type, but @racket[(List B ... String)] (any number of
  @racket[B]s followed by a @racket[String]) is not. A dotted type variable can
  also be used to describe the type of a variadic function, as long as it
  appears after all other arguments, so @racket[(→ String B ... Void)] (a
  function taking a @racket[String], any number of @racket[B]s, and returning
  @racket[Void]) is a valid type, but @racket[(→ String B ... Void)] (a function
  taking any number of @racket[B]s, and a @racket[String], and returning
  @racket[Void]) is not. Finally, a dotted type variable can be used to
  represent the last element of the tuple of returned values, for functions
  which return multiple values (which are described below).@htodo{multiple
   values is described after, not cool.}

  When the context makes it unclear whether an ellipsis @${…} indicates a
  dotted type variable, or is used to indicate a metasyntactic repetition at the
  level of mathematical formulas, we will write the first using @${τ₁ … τₙ},
  explicitly indicating the first and last elements, or using @${\overline{τ}}
  @todo{and we will write the second using @${\textit{tvar}\ @$ooo}}

  @;{
   @todo{and we will write the second using @${\textit{tvar}\ \textit{ooo}}}

   @todo{and we will write the second using @${\textit{tvar}\ \textit{ddd}}}

   @todo{and we will write the second using @${\textit{tvar}\ ⋯}}

   @todo{and we will write the second using @${\textit{tvar}\ ⋰}}

   @todo{and we will write the second using @${\textit{tvar}\ ⋱}}

   @todo{and we will write the second using @${\textit{tvar}\ ⋮}}

   @todo{and we will write the second using @${\textit{tvar}\ _{***}}}

   @todo{and we will write the second using
    @${\textit{tvar}\ _{\circ\circ\circ}}.}
  }

  Functions in Racket can return one or several values. When the number of
  values returned is different from one, the result tuple can be destructured
  using special functions such as @racket[(call-with-values _f _g)], which
  passes each value returned by @racket[_f] as a distinct argument to
  @racket[_g]. The special form @racket[(let-values ([(_v₁ … _vₙ) _e]) _body)]
  binds each value returned by @racket[_e] to the corresponding @racket[_vᵢ]
  (the expression @racket[_e] must produce exactly @racket[n] values). The type
  of a function returning multiple values can be expressed using the following
  notation:

  @racket[(→ In₁ … Inₙ (Values Out₁ … Outₘ))]

  @htodo{Something on which types can be inferred and which can't (for now).}

  Finally, predicates (functions whose results can be interpreted as booleans)
  can be used to gain information about the type of their argument, depending on
  the result. The type of a predicate can include positive and negative
  @deftech{filters}, indicated with @racket[#:+] and @racket[#:-], respectively.
  The type of the @racket[string?] predicate is:

  @racketblock[(→ Any Boolean : #:+ String #:- (! String))]

  In this notation, the positive filter @racket[#:+ String] indicates that when
  the predicate returns @racket[#true], the argument is known to be a
  @racket[String]. Conversely, when the predicate exits with @racket[#false],
  the negative filter @racket[#:- (! String)] indicates that the input could not
  (@racket[!]) possibly have been a string. The information gained this way
  allows regular conditionals based on arbitrary predicates to work like
  pattern-matching:

  @examples[#:label #f #:eval (tr-eval)
            (define (f [x : (U String Number Symbol)])
              (if (string? x)
                  (code:comment "x is known to be a String here:")
                  (ann x String)
                  (code:comment "x is known to be a Number or a Symbol here:")
                  (ann x (U Number Symbol))))]

  The propositions do not necessarily need to refer to the value as a whole,
  and can instead give information about a sub-part of the value. Right now, the
  user interface for specifying paths can only target the left and right members
  of @racket[cons] pairs, recursively. Internally, @|typedracket| supports
  richer paths, and the type inference can produce filters which give
  information about individual structure fields, or about the result of forced
  promises, for example.}

 @asection{
  @atitle{Occurrence typing}

  @|Typedracket| is built atop @racket[Racket], which does not natively support
  pattern matching. Instead, pattern matching forms are implemented as macros,
  and expand to nested uses of @racket[if].

  As a result, @|typedracket| needs to typecheck code with the following
  structure:

  @racketblock[
 (λ ([v : (U Number String)])
   (if (string? v)
       (string-append v ".")
       (+ v 1)))]

  In this short example, the type of @racket[v] is a union type including
  @racket[Number] and @racket[String]. After applying the @racket[string?]
  predicate, the type of @racket[v] is narrowed down to @racket[String] in the
  @emph{then} branch, and it is narrowed down to @racket[Number] in the @emph{
   else} branch. The type information gained thanks to the predicate comes from
  the @tech{filter} part of the predicate's type (as explained in
  @secref["tr-presentation-functions"]).

  Occurrence typing only works on immutable variables and values. Indeed, if
  the variable is modified with @racket[set!], or if the subpart of the value
  stored within which is targeted by the predicate is mutable, it is possible
  for that value to change between the moment the predicate is executed, and the
  moment when the value is actually used. This places a strong incentive to
  mostly use immutable variables and values in @|typedracket| programs, so that
  pattern-matching and other forms work well.

  In principle, it is always possible to copy the contents of a mutated
  variable to a temporary one (or copy a mutable subpart of the value to a new
  temporary variable), and use the predicate on that temporary copy. The code in
  the @emph{then} and @emph{else} branches should also use the temporary copy,
  to benefit from the typing information gained via the predicate. In our
  experience, however, it seems that most macros which perform tasks similar to
  pattern-matching do not provide an easy means to achieve this copy. It
  therefore remains more practical to avoid mutation altogether when possible.}

 @asection{
  @atitle[#:tag "tr-presentation-recursive-types"]{Recursive types}
   
  @|Typedracket| allows recursive types, both via (possibly mutually-recursive)
  named declarations, and via the @racket[Rec] type operator.

  In the following examples, the types @racket[Foo] and @racket[Bar] are
  mutually recursive. The type @racket[Foo] matches lists with an even number of
  alternating @racket[Integer] and @racket[String] elements, starting with an
  @racket[Integer],

  @racketblock[
 (define-type Foo (Pairof Integer Bar))
 (define-type Bar (Pairof String (U Foo Null)))]

  This same type could alternatively be defined using the @racket[Rec]
  operator. The notation @racket[(Rec R T)] builds the type @racket[T], where
  occurrences of @racket[R] are interpreted as recursive occurrences of
  @racket[T] itself.
  
  @racketblock[
 (Rec R
      (Pairof Integer
              (Pairof String
                      (U R Null))))]}

 @asection{
  @atitle{Classes}

  The @racketmodname[racket/class] module provides an object-oriented system
  for Racket. It supports the definition of a hierarchy of classes with single
  inheritance, interfaces with multiple inheritance, mixins and traits (methods
  and fields which may be injected at compile-time into other classes), method
  renaming, and other features.

  The @racketmodname[typed/racket/class] module makes most of the features of
  @racketmodname[racket/class] available to @|typedracket|. In particular, it
  defines the following type operators:

  @itemlist[
 @item{@racket[Class] is used to describe the features a class, including the
    signature of its constructor, as well as the public fields and methods
    exposed by the class. We will note that a type expressed with @racket[Class]
    does not mention the name of the class. Any concrete implementation which
    exposes all (and only) the required methods, fields and constructor will
    inhabit the type. In other words, the types built with @racket[Class] are
    structural, and not nominal.
   }
 @item{@racket[Object] is used to describe the methods and fields which an
    already-built object bears.}
 @item{The @racket[(Instance (Class …))] type is a shorthand for the
    @racket[Object] type of instances of the given class type. It can be useful
    to describe the type of an instance of a class without repeating all the
    fields and methods (which could have been declared elsewhere).}
 @item{In types described using @racket[Class] and @racket[Instance], it is
    possible to omit fields which are not relevant. These fields get grouped
    under a single @emph{row polymorphic} type variable. A row polymorphic
    function can, for example, accept a class with some existing fields, and
    produce a new class extending the existing one:

    @(let ([ev (tr-eval)])
       @list{
     @examples[#:label #f #:eval ev
               (: add-my-field (∀ (r #:row)
                                  (-> (Class (field [existing Number])
                                             #:row-var r)
                                      (Class (field [existing Number]
                                                    [my-field String])
                                             #:row-var r))))
               (define (add-my-field parent%)
                 (class parent%
                   (super-new)
                   (field [my-field : String "Hello"])))]

     The small snippet of code above defined a function @racket[add-my-field]
     which accepts a @racket[parent%] class exporting at least the
     @racket[existing] field (and possibly other fields and methods). It then
     creates an returns a subclass of the given @racket[parent%] class, extended
     with the @racket[my-field] field.

     We consider the following class, with the required @racket[existing] field,
     and a supplementary @racket[other] field:

     @examples[#:label #f #:eval ev
               (define a-class%
                 (class object%
                   (super-new)
                   (field [existing : Integer 0]
                          [other : Boolean #true])))]

     When passed to the @racket[add-my-field] function, the row type variable is
     implicitly instantiated with the field @racket[[other Boolean]]. The result
     of that function call is therefore a class with the three fields
     @racket[existing], @racket[my-field] and @racket[other].

     @examples[#:label #f #:eval ev
               (add-my-field a-class%)]

     These mechanisms can be used to perform reflective operations on classes
     like adding new fields and methods to dynamically-created subclasses, in a
     type-safe fashion.

     The @racket[Row] operator can be used along with @racket[row-inst] to
     explicitly instantiate a row type variable to a specific set of fields and
     methods. The following call to @racket[add-my-field] is equivalent to the
     preceding one, but does not rely on the automatic inference of the row type
     variable.

     @examples[#:label #f #:eval ev
               ({row-inst add-my-field (Row (field [other Boolean]))} a-class%)]
     })}]

  We will not further describe this object system here, as our work does not
  rely on this aspect of @|typedracket|'s type system. We invite the curious
  reader to refer to the documentation for @racketmodname[racket/class] and
  @racketmodname[typed/racket/class] for more details.

  We will simply note one feature which is so far missing from @|typedracket|'s
  object system: immutability. It is not possible yet to indicate in the type of
  a class that a field is immutable, or that a method is pure (in the sense of
  always returning the same value given the same input arguments). The absence
  of immutability means that occurrence typing does not work on fields. After
  testing the value of a field against a predicate, it is not possible to narrow
  the type of that field, because it could be mutated by a third party between
  the check and future uses of the field.
 }

 @asection{
  @atitle{Local type inference}

  Unlike many other statically typed functional languages, @|typedracket| does
  not rely on a Hindley–Milner type system@~cite["HMMilner78" "HMHindley69"].
  This choice was made for several reasons@~cite["tobin-hochstadt_typed_2010"]:
  @itemlist[
 @item{@|Typedracket|'s type system is rich and contains many features. Among
    other things, it mixes polymorphism and subtyping, which notoriously make
    typechecking difficult.}
 @item{The authors of @|typedracket| claim that global type inference often
    produces indecipherable error messages, with a small change having
    repercussions on the type of terms located in other distant parts of the
    program.}
 @item{The authors of @|typedracket| suggest that type annotations are often
    beneficial as documentation. Since the type annotations are checked at
    compile-time, they additionally will stay synchronised with the code that
    they describe, and will not become out of date.}]

  Instead of relying on global type inference, @|typedracket| uses local type
  inference to determine the type of local variables and expressions.
  @|Typedracket|'s type inference can also determine the correct instantiation
  for most calls to polymorphic functions. It however requires type annotations
  in some places. For example, it is usually necessary to indicate the type of
  the parameters when defining a function.}
 @htodo{Vectors}
}

@(begin
   (define (aligned #:valign [valign 'mid] . lines)
     (define valign-letter (case valign [(top) "t"] [(mid) "m"] [(bot) "b"]))
     @list{
 \begin{aligned}[@valign-letter]
 @lines
 \end{aligned}
}
     )

   (define acase list)
   (define cases
     (λ (#:first-sep [first-sep "\\vphantom{x}\\mathbin{:=}\\vphantom{x}"]
         #:then-sep [then-sep "|\\;\\ "] term
         . the-cases)
       (list
        term
        (aligned #:valign 'top @; cl
                 @(for/list ([c (in-list the-cases)]
                             [i (in-naturals)])
                    (list (if (= i 0) first-sep then-sep)
                          " & "
                          c
                          (if (= i (sub1 (length the-cases))) "" "\\\\\n")))
                 ))))
   (define (frac x . y)
     @list{\frac{@x}{@y}})
   (define where @${\text{ where }})
   (define textif @${\text{ if }})
   (define quad @${\quad})

   (define (cat x . y)
     (if (null? y)
         @list{\mathsf{@x}}
         @list{\mathsf{@x}\ @y}))
   (define ℂ∞ @${\overline{ℂ}})
   (define tvarset @${V})
   (define u𝕋 @${𝕋_@tvarset})
   (define u𝕋∅ @${𝕋_∅})
   (define (τ x) @${τ(\textit{@x})})

   (define (τrule v t #:& [& #t])
     (if &
         @${@v & ∈ τ(@t)}
         @${@v ∈ τ(@t)})))

@asection{
 @atitle{Formal semantics for part of Typed Racket's type system}

 We define the universe of values that can be created manipulated with
 @|typedracket| as follows:

 @$${
  @cases["𝔻"
         @acase{@cat["num"]{c} ∈ @ℂ∞}
         @acase{@cat["chr"]{h} ∈ ℍ}
         @acase{@cat["str"]{s} ∈ 𝕊}
         @acase{@cat["sym"]{y} ∈ 𝕐}
         @acase{@cat["fun"]{f} ∈ 𝔽}
         @acase{@cat["pair"](d, d') @where d,d' ∈ 𝔻}
         @acase{@cat["vec"](d₁, …, dₙ) @where dᵢ ∈ 𝔻, n ∈ ℕ}
         @acase{@cat["null"]}
         @acase{@cat["void"]}
         @acase{@cat["true"] ∈ 𝟙}
         @acase{@cat["false"] ∈ 𝟙}
         @acase{@cat["struct"](f₁ = d₁, …, fₙ = dₙ)
            @where fᵢ ∈ ℱ, dᵢ ∈ 𝔻}]
 }

 where @ℂ∞ is the subset of complex numbers that can be represented in
 Racket, extended with a few values like floating-point infinities and ``not a
 number'' special values@note{More precisely Racket can represent complex
  rationals with arbitrary precision (i.e. numbers of the form @${@frac["a" "b"]
   + @frac["c" "d"]i} where @${a,b,c,d ∈ ℤ ∧ b,d ≠ 0} and have a small enough
  magnitude to be represented without running out of memory), as well as
  complex numbers where both components are either single-precision or
  double-precision IEEE floating-point numbers, including special values like
  positive and negative infinity (for double-precision floats: @racket[+inf.0]
  and @racket[-inf.0]), positive and negative zero (@racket[+0.0] and
  @racket[-0.0]) and positive and negative ``not a number'' values
  (@racket[+nan.0] and @racket[-nan.0]).}.

 @${ℍ} is the set of characters that can be represented in Racket@note{That
  is, all the Unicode code points in the ranges 0 – d7ff@subscript{16} and
  e000@subscript{16} – 10ffff@subscript{16}}.

 @${𝕊} is the set of strings based on characters present in @${ℍ}.@htodo{is
  this a free monoid or something?}

 @${𝕐} is the set of symbols that can be manipulated in Racket. It includes
 interned symbols (which are identified by their string representation), and
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{uninterned}
 symbols which are different from all other symbols, including those with the
 same string representation@note{We will not consider
  @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{unreadable
   symbols}, whose behaviour is inbetween@htodo{This sentence sounds weird}.}.

 @${𝔽} is the universe of Racket functions. A function @${f ∈ 𝔽} is a partial
 function from tuples of arguments to tuples of return values.

 @$${𝔽 = 𝔻ⁿ ↛ 𝔻ᵐ @where n,m ∈ ℕ}

 @${ℕ} is the set of natural integers.

 @${𝟙} is the universe of booleans, which only contains the values
 @${@cat["true"]} and @${@cat["false"]}.

 @${ℱ} is the universe of field names.

 We voluntarily omit some more exotic data types such as
 @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{byte strings} (indexed
 strings of bytes, compared to the indexed strings of unicode characters
 presented above),
 @tech[#:key "regexp" #:doc '(lib "scribblings/guide/guide.scrbl")]{regular
  expressions} and preexisting opaque structure types from Racket's standard
 library (@racket[Subprocess], for example, which represent a running
 subprocess on which a few actions are available). We also do not consider
 mutable strings and pairs, which exist in @|typedracket| for backwards
 compatibility, but which are seldom used in practice.

 @todo{Value-belongs-to-type relationship:}

 We define a universe of types @u𝕋 parameterized by @${@tvarset ⊆ 𝕧}, which
 indicates the set of free variables which may occur in the type. We note
 individual types as @${τ(\textit{Type})}, where @${Type} is the name of the
 type being considered. Unless otherwise specified, @${τ(\textit{Type}) ∈
  @|u𝕋|\ ∀ @tvarset}. @todo{The previous sentences are a bit fuzzy.} The
 universe of types with no free variables is @${@u𝕋∅ ⊆ \mathcal{P}(𝔻)}.

 @$${
  \begin{gathered}
   \textit{tvar} ∈ @tvarset ⇒ τ(\textit{tvar}) ∈ @u𝕋 \\
   @u𝕋∅ ⊆ \mathcal{P}(𝔻)
  \end{gathered}
 }


 Values belong to their singleton type. We define a type inhabited by a single
 value @${v} with the notation @${τ(@cat["cat"]{v})}, where @${@cat["cat"]{}}
 indicates the ``category'' of the value (whether it is a number, a string, a
 function, a boolean…).

 @$${
  @aligned{
   @τrule[@cat["num"]{c} @cat["num"]{c}] \\
   @τrule[@cat["chr"]{h} @cat["chr"]{h}] \\
   @τrule[@cat["str"]{s} @cat["str"]{s}] \\
   @τrule[@cat["sym"]{y} @cat["sym"]{y}] \\
   @τrule[@cat["true"] @cat["true"]] \\
   @τrule[@cat["false"] @cat["false"]] \\
   @τrule[@cat["null"] @${\textit{Null}}] \\
   @τrule[@cat["void"] @${\textit{Void}}]
  }
 }

 These simple values also belong to their wider type, which we note as
 @;
 @${τ(\textit{Typename})}.

 @$${
  @aligned{
   @τrule[@cat["num"]{c} @${\textit{Number}}] &⊂ @τ{Any} \\
   @τrule[@cat["chr"]{h} @${\textit{Char}}] &⊂ @τ{Any} \\
   @τrule[@cat["str"]{s} @${\textit{String}}] &⊂ @τ{Any} \\
   @τrule[@cat["sym"]{y} @${\textit{Symbol}}] &⊂ @τ{Any} \\
   @τrule[@cat["true"] @${\textit{Boolean}}] &⊂ @τ{Any} \\
   @τrule[@cat["false"] @${\textit{Boolean}}] &⊂ @τ{Any} \\
   @cat["null"] & &⊂ @τ{Any} \\
   @cat["void"] & &⊂ @τ{Any}
  }
 }

 We give the type of pairs and vector values below:

 @$${
  @aligned{
   @τrule[@${@cat["pair"](a, b)} @${\textit{Pairof A B}}]
   &&@textif a ∈ τ(A) ∧ b ∈ τ(B) \\
   @τrule[@${@cat["vec"](a₁, …, aₙ)} @${\textit{Vector A₁ … Aₙ}}]
   &&@textif aᵢ ∈ τ(Aᵢ)\ ∀ i
  }
 }

 The type @${τ(\textit{List}\ A₁\ …\ Aₙ)} is a shorthand for describing the
 type of linked lists of pairs of fixed length:

 @$${
  @aligned{
   τ(\textit{List}\ A\ \overline{B})
   &= τ(\textit{Pairof}\ A₁\ (List\ \overline{B})) \\
   @where \text{$\overline{B} is a placeholder for any number of types$}
   τ(\textit{List}) &= τ(Null)
  }
 }

 More general types exist for linked lists of pairs and vectors of unknown
 length:

 @$${
  @aligned{
   @τrule[@cat["null"] @${\textit{Listof}\ A}]\ ∀\ A \\
   @τrule[@${@cat["pair"](a, b)} @${\textit{Listof}\ A}]
   && @textif a ∈ τ(A) ∧ b ∈ τ(\textit{Listof}\ A) \\
   @τrule[@${@cat["vec"](a₁, …, aₙ)} @${\textit{Vectorof}\ A}]
   && @textif aᵢ ∈ τ(A)
  }
 } 

 There are a few intermediate types between singleton types for individual
 numbers and
 @;
 @${τ(\textit{Number})}. We show a few of these below. The other types which are
 part of @racket[typedracket]'s numeric tower are defined in the same way, and
 are omitted here for conciseness.

 @$${
  @aligned{
   @τrule[@cat["num"]{c} @${\textit{Positive-Integer}}]
   && @textif c ∈ ℕ ∧ c > 0 \\
   @τrule[@cat["num"]{c} @${\textit{Nonnegative-Integer}}]
   && @textif c ∈ ℕ ∧ c ≥ 0 \\
   @τrule[@cat["num"]{c} @${\textit{Nonpositive-Integer}}]
   && @textif c ∈ ℕ ∧ c ≤ 0 \\
   @τrule[@cat["num"]{0} @${\textit{Zero}}] \\
   @τrule[@cat["num"]{1} @${\textit{One}}] &
  }
 }

 Functions types are inhabited by functions which accept arguments of the
 correct type, and return a tuple of values belonging to the expected result
 type. We do not take into consideration the possible side effects of the
 function here, partly because our compiler-writing framework seldom uses
 mutation (at the run-time phase of the program).

 @$${
  @aligned{
   &@τrule[#:& #f @cat["fun"]{f} @${τ₁, …, τₙ → (\textit{Values} τ'₁, …, τ'ₘ)}]
   \\
   &@|quad|@textif
   vᵢ ∈ τᵢ ∀ i ⇒ (v₁, …, vₙ) ∈ dom(f) ∧ f(v₁, …, vₙ) ∈ (τ'₁, …, τ'ₘ) \\
   &@|quad|@where
   (o₁, …, oₘ) ∈ (τ'₁, …, τ'ₘ) @textif oᵢ ∈ τ'ᵢ\\
  }
 }

 For polymorphic functions, we define a @${\operatorname{freetvars}(t)}
 operator, which returns the set of bound variables accessible within a given
 type @todo{This is backwards: we did not define well what it means for a bound
 variable to be accessible.}
 
 @$${t ∈ @u𝕋 ⇒ boundvars(t) = @tvarset}

 @todo{We should not have the @${@textif τᵢ, τ'ⱼ ∈ 𝕋_{@|tvarset|⁺}} clause
  below, instead we should define the notion of well-scopedness of a type.}
 
 @$${
  @aligned{
   &@cat["fun"]{f} ∈ t = τ(∀\ \textit{tvar₁}\ …\ \textit{tvarₖ}
   \ (τ₁ … τₙ → (Values τ'₁ … τ'ₘ)))\\
   &@|quad|@where @tvarset = \operatorname{boundtvars}(t) \\
   &@|quad|@where @|tvarset|⁺ = @tvarset ∪ \{\textit{tvar₁} … \textit{tvarₖ}\}
   \\
   &@|quad|@textif τᵢ, τ'ⱼ ∈ 𝕋_{@|tvarset|⁺} \\
   @;TODO: make @u𝕋 take an argument
   &@|quad|@textif
   @aligned[#:valign 'top]{
    ∀ \textit{inst₁}, …, \textit{instₖ} ∈ @|u𝕋|, f
    ∈ τ(σ(τ₁)\ …\ σ(τₙ) → (Values\ σ(τ'₁)\ …\ σ(τ'ₘ)))
   } \\
   &@|quad|@where σ(τ) = τ[\textit{tvarᵢ} ↦ \textit{instᵢ} …]
  }
 }

 where the notation @${τ[a₁ ↦ b₁ … aₙ ↦ bₙ]} indicates the substitution within
 @${τ} of all occurrences of @${aᵢ} with the corresponding @${bᵢ}. The
 substitutions are performed in parallel.

 @todo{if or iff for the function's types above?}

 @todo{other function types}

 @todo{dotted function types (variadic polymorphic types)}

 @todo{Vectorof, Listof}

 @todo{Intersections}

 @todo{is the notation for tuples of values returned by functions okay?}

 @todo{A function cannot forge a value of type @racket[A], where @racket[A] is
  a polymorphic type variable. It must return an input value with the desired
  type (or exit with an error, in which case the function's actual return type
  is @racket[Nothing]).}

 @htodo{something else I forgot?}

 The association with types and values given above naturally yields the
 subtyping relationship @tr≤: explicited below:

 @$${
  @aligned{
   @τ{T} & @tr≤: @τ{T}\ ∀\ T & \\
   @τ{Nothing} & @tr≤: @τ{T}\ ∀\ T & \\
   τ(@cat["num"]{n}) & @tr≤: @τ{Number} & \\
   τ(@cat["chr"]{h}) & @tr≤: @τ{Char} & \\
   τ(@cat["str"]{s}) & @tr≤: @τ{String} & \\
   τ(@cat["sym"]{y}) & @tr≤: @τ{Symbol} & \\
   τ(@cat["true"]) & @tr≤: @τ{Boolean} & \\
   τ(@cat["false"]) & @tr≤: @τ{Boolean} & \\[1ex]
   τ(A) & @tr≤: τ(U\ A\ B\ …) & \\
   τ(A₁ … Aₙ → B₁ … Bₘ) & @tr≤: τ(A'₁ … A'ₙ → B'₁ … B'ₘ) & \\
   & @textif A'ᵢ @tr≤: Aᵢ ∧ Bᵢ @tr≤: B'ᵢ & \\
   … & @tr≤: … & \\[1ex]
   @τ{T} & @tr≤: @τ{Any}\ ∀\ T &
  }
 }
}