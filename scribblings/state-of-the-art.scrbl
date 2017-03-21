#lang scribble/manual

@require["util.rkt"]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{State of the art}

@asection{
 @atitle{Extending the type system via macros (type-expander)}

 Our work explores one interesting use of macros: their use to extend a
 programming language's type system.

 Chang, Knauth and Greenman@~cite{chang2017type-systems-as-macros} took
 the decision to depart from Typed Racket, and implemented a new approach,
 which allows type systems to be implemented as macros. Typing information
 about identifiers is threaded across the program at compile-time, and macros
 can decide whether a term is well-typed or not.

 Another related project is Cur@note{@url{https://github.com/wilbowma/cur}}, a
 dependent type system implemented using Racket macros.

 Bracha suggests that pluggable type systems should be
 used@~cite{bracha2004pluggable-types}. Such a system, JavaCOP is presented by
 Andreae, Noble, Markstrum and
 Shane@~cite{pluggable-types-andreae2006framework} as a tool which ``enforces
 user-defined typing constraints written in a declarative and expressive rule
 language''.

 In contrast, Typed Racket@~cite{tobin-hochstadt_design_2008} was
 implemented using macros on top of ``untyped'' Racket, but was not
 designed as an extensible system: new rules in the type system must be
 added to the core implementation, a system which is complex to approach.

 Following work by Asumu
 Takikawa@note{@url{https://github.com/racket/racket/pull/604}}, we
 extended Typed Racket with support for macros in the type declarations and
 annotations. We call this sort of macro ``type expanders'', following the
 commonly-used naming convention (e.g. ``match expanders'' are macros which
 operate within patterns in pattern-matching forms). These type expanders
 allow users to easily extend the type system with new kinds of types, as
 long as these can be translated back to the types offered natively by
 Typed Racket.

 While the Type Systems as Macros by Chang, Knauth and
 Greenman@~cite{chang2017type-systems-as-macros} allows greater flexibility
 by not relying on a fixed set of core types, it also places on the
 programmer the burden of ensuring that the type checking macros are
 sound. In contrast, our type expanders rely on Typed Racket's type
 checker, which will still catch type errors in the fully-expanded
 types. In other words, writing type expanders is safe, because they do not
 require any specific trust, and translate down to plain Typed Racket
 types, where any type error would be caught at that level.

 An interesting aspect of our work is that the support for type expanders
 was implemented without any change to the core of Typed Racket. Instead,
 the support for type expanders itself is available as a library, which
 overrides special forms like @tt{define}, @tt{lambda} or
 @tt{cast}, enhancing them by pre-processing type expanders, and
 translating back to the ``official'' forms from Typed Racket. It is worth
 noting that Typed Racket itself is implemented in a similar way: special
 forms like @tt{define} and @tt{lambda} support plain type
 annotations, and translate back to the ``official'' forms from so-called
 ``untyped'' Racket. In both cases, this approach goes with the Racket
 spirit of implementing languages as
 libraries@~cite{tobin-hochstadt_languages_as_libraries_2011}}

  @asection{
 @atitle{Algebraic datatypes for compilers (phc-adt)}
    The @tt{phc-adt} library implements algebraic datatypes (variants and
    structures) which are adapted to compiler-writing.

    There is an existing ``simple'' datatype library for Typed/Racket (source
    code available at @url{https://github.com/pnwamk/datatype}). It differs
    from our library on several points:
    @itemlist[
    @item{``datatype'' uses nominal typing, while we use structural typing
      (i.e. two identical type declarations in distinct modules yield the same
      type in our case). This avoids the need to centralize the type
      definition of ASTs.}
    @item{``datatype'' uses closed variants, where a constructor can only
      belong to one variant. We simply define variants as a union of
      constructors, where a constructor can belong to several variants. This
      allows later passes in the compiler to add or remove cases of variants,
      without the need to duplicate all the constructors under slightly
      different names.}
    @item{``datatype'' does not support row polymorphism, or similarly the
      update and extension of its product types with values for existing and
      new fields, regardless of optional fields. We implement the
      latter.@htodo{ Probably the former too.}}]

    @asection{
 @atitle{The case for bounded row polymorphism}
      @todo{Explain the ``expression problem''.}

      We strive to implement compilers using many passes. A pass should be
      able to accept a real-world AST, and produce accordingly a real-world
      transformed AST as its output. It should also be possible to use
      lightweight ``mock'' ASTs, containing only the values relevant to the
      passes under test (possibly only the pass being called, or multiple
      passes tested from end to end). The pass should then return a
      corresponding simplified output AST, omitting the fields which are
      irrelevant to this pass (and were not part of the input). Since the
      results of the pass on a real input and on a test input are equivalent
      modulo the irrelevant fields, this allows testing the pass in isolation
      with simple data, without having to fill in each irrelevant field with
      @tt{null} (and hope that they are indeed irrelevant, without a
      comforting guarantee that this is the case), as is commonly done when
      creating ``mocks'' to test object-oriented programs.

      This can be identified as a problem similar to the ``expression
      problem''. In our context, we do not strive to build a compiler which
      can be extended in an ``open world'', by adding new node types to the
      AST, or new operations handling these nodes. Rather, the desired outcome
      is to allow passes to support multiple known subsets of the same AST
      type, from the start.

      We succinctly list below some of the different sorts of polymorphism,
      and argue that row polymorphism is well-suited for our purpose. More
      specifically, bounded row polymorphism gives the flexibility needed when
      defining passes which keep some fields intact (without reading their
      value), but the boundedness ensures that changes in the input type of a
      pass do not go unnoticed, so that the pass can be amended to handle the
      new information, or its type can be updated to ignore this new
      information.

      @subsubsub*section{Subtyping, polymorphism and alternatives}
      @|~|@(linebreak)
      @~cite{ducournau-cours-lots}.  @htodo{The course includes a couple of
        other kinds of polymorphism and subtyping (or makes finer distinctions
        than in the list below). Refine and augment the list below using
        Roland's classification.} @htodo{Probably also cite:}
      @~cite{cardelli1985understanding} @htodo{(apparently does not cover
      ``Higher-Kinded Polymorphism'', ``Structural Polymorphism'' and ``Row
      Polymorphism'')}
      @itemlist[
      @item{Subtyping (also called inclusion polymorphism, subtype
        polymorphism, or nominal subtyping ?): Subclasses and interfaces in
        @csharp and Java, sub-structs and union types in Typed Racket,
        polymorphic variants in @CAML @~cite[#:precision @list{chap. 6, sec. Polymorphic Variants}]{minsky2013real}}
        @; https://realworldocaml.org/v1/en/html/variants.html
        
        @; See also: use of exceptions as dynamically extensible sum types:
        @; http://caml-list.inria.narkive.com/VJcoGfvp/dynamically-extensible-sum-types
        @; 
        @; quote: I need a dynamically extensible sum type. I can think of three approaches:
        @; quote: 
        @; quote: (1) Use polymorphic variants: `Foo of a * b, `Bar of c * d * e, etc
        @; quote: 
        @; quote: (2) Use exceptions: exception Foo of a * b, exception Bar of c * d * e, etc
        @; quote: 
        @; quote: (3) Use thunks: (fun () -> foo a b), (fun () -> bar c d e), etc
        @; quote: 
        @; quote: Using exceptions seems somewhat sneaky to me. Does it have any advantages over
        @; quote: polymorphic variants? The polymorphic variants seem like they might be better
        @; quote: since you could actually limit the domain of certain functions... thus, one
        @; quote: part of your program could be constrained to a subrange of the sum type, while
        @; quote: other parts could be opened up fully.
        @; quote: 
        @; quote: Until now I have been using the thunking approach in an event-based
        @; quote: architecture (each event on the queue is a unit->unit thunk). This seems to
        @; quote: work pretty well. But now I'm thinking that the other approaches would allow
        @; quote: arbitrary filters to be applied to events; i.e., the thunk approach imposes a
        @; quote: "read once" discipline on elements of the sum type, and in some applications
        @; quote: you might want "read multiple".
        @; quote: 
        @; quote: I'm not asking the question solely in terms of event-based architectures,
        @; quote: though, and I'm interested in others experience with the different approaches
        @; quote: to dynamically extensible sum types, and what led you to choose one approach
        @; quote: over the others. Thanks!
      @item{Multiple inheritance. @NIT, @CLOS, @CPP, @csharp interfaces, Java
        interfaces. As an extension in ``untyped'' Racket with Alexis King's
        safe multimethods@note{@url{https://lexi-lambda.github.io/blog/2016/02/18/simple-safe-multimethods-in-racket/}}.
        
        This in principle could help in our case: AST nodes would have
        @tt{.withField(value)} methods returning a copy of the node with
        the field's value updated, or a node of a different type with that
        new field, if it is not present in the initial node type. This would
        however require the declaration of many such methods in advance, so
        that they can be used when needed (or with a recording mechanism
        like the one we use, so that between compilations the methods called
        are remembered and generated on the fly by a macro). Furthermore,
        @typedracket lacks support for multiple inheritance on structs. It
        supports multiple inheritance for classes @todo{?}, but classes
        currently lack the ability to declare immutable fields, which in
        turn causes problems with occurrence typing (see the note in the
        ``row polymorphism'' point below).}
      @item{Parametric polymorphism: Generics in @csharp and Java,
        polymorphic types in @CAML and @typedracket}
      @item{F-bounded polymorphism: Java, @csharp, @CPP, Eiffel. Possible
        to mimic to some extent in @typedracket with (unbounded) parametric
        polymorphism and intersection types.
        @todo{Discuss how it would work/not work in our case.}}
      @item{Operator overloading (also called overloading polymorphism?) and
        multiple dispatch:
        @itemlist[
        @item{Operator overloading in @csharp}
        @item{Nothing in Java aside from the built-in cases for arithmetic and string concatenation, but those are not extensible}
        @item{@CPP}
        @item{typeclasses in Haskell? @todo{I'm not proficient enough in Haskell to
          be sure or give a detailed description, I have to ask around to double-check.}}
        @item{LISP (CLOS): has multiple dispatch}
        @item{nothing built-in in @|typedracket|.}
        @item{@|CAML|?}]}
      @item{Coercion polymorphism (automatic casts to a given type). This
        includes Scala's implicits, @csharp implicit coercion operators
        (user-extensible, but at most one coercion operator is applied
        automatically, so if there is a coercion operator @${A → B},
        and a coercion operator @${B → C}, it is still impossible to
        supply an @${A} where a @${C} is expected without manually coercing the
        value), and @CPP's implicit conversions, where single-argument
        constructors are treated as implicit conversion operators, unless
        annotated with the @tt{explicit} keyword.  Similarly to @csharp,
        @CPP allows only one implicit conversion, not two or more in a chain.

        Struct type properties in untyped Racket can somewhat be used to that
        effect, although they are closer to Java's interfaces than to coercion
        polymorphism. Struct type properties are unsound in @typedracket and are
        not represented within the type system, so their use is subject to caution
        anyway.}
      @item{Coercion (downcasts). Present in most typed languages. This
        would not help in our case, as the different AST types are
        incomparable (especially since @typedracket lacks multiple inheritance)}

        
      @item{Higher-kinded polymorphism: Type which is parameterized by a
        @${\mathit{Type} → \mathit{Type}} function. Scala, Haskell. Maybe
        @|CAML|?

        The type expander library which we developed for @typedracket
        supports @${Λ}, used to describe anonymous type-level
        macros. They enable some limited form of
        @${\mathit{Type} → \mathit{Type}} functions, but are
        actually applied at macro-expansion time, before typechecking is
        performed, which diminishes their use in some cases. For example,
        they cannot cooperate with type inference. Also, any recursive use
        of type-level macros must terminate, unless the type ``function''
        manually falls back to using @${\mathit{Rec}} to create a regular
        recursive type. This means that a declaration like
        @${F(X) := X × F(F(X))} is not possible using anonymous type-level
        macros only.
        @; See https://en.wikipedia.org/wiki/Recursive_data_type#Isorecursive_types
        @; Is this a matter of isorecursive vs equirecursive ?

        As an example of this use of the type expander library, our
        cycle-handling code uses internally a ``type traversal'' macro. In
        the type of a node, it performs a substitution on some subparts of
        the type. It is more or less a limited form of application of a
        whole family of type functions @${aᵢ → bᵢ}, which have the same inputs
        @${aᵢ …}, part of the initial type, but different outputs @${bᵢ …} which
        are substituted in place of the @${aᵢ …} in the resulting type. The
        ``type traversal'' macro expands the initial type into a standard
        polymorphic type, which accepts the desired outputs @${bᵢ …} as type
        arguments.}
      @item{Lenses. Can be in a way compared to explicit coercions, where
        the coercion is reversible and the accessible parts can be altered.}
      @item{Structural polymorphism (also sometimes called static duck-typing): Scala,
        TypeScript. It is also possible in @typedracket, using the algebraic
        datatypes library which we implemented. Possible to mimic in Java
        and @csharp with interfaces ``selecting'' the desired fields, but
        the interface has to be explicitly implemented by the class (i.e. at
        the definition site, not at the use-site).

        Palmer et al. present TinyBang@~cite{types-for-flexible-objects}, a
        typed language in which flexible manipulation of objects is possible,
        including adding and removing fields, as well as changing the type of
        a field. They implement in this way a sound, decidable form of static
        duck typing, with functional updates which can add new fields and
        replace the value of existing fields. Their approach is based on two
        main aspects:
        @itemlist[
        @item{@emph{Type-indexed records supporting asymmetric concatenation}:
          by concatenating two records @${r₁ \& r₂}, a new record is obtained
          containing all the fields from @${r₁} (associated to their value in
          @${r₁}), as well as the fields from @${r₂} which do not appear in @${r₁}
          (associated to their value in @${r₂}). Primitive types are eliminated
          by allowing the use of type names as keys in the records: integers
          then become simply records with a @${int} key, for example.}
        @item{@emph{Dependently-typed first-class cases}: pattern-matching
          functions are expressed as
          @${pattern \mathbin{\texttt{->}} expression}, and can be concatenated
          with the @${\&} operator, to obtain functions matching against
          different cases, possibly with a different result type for each
          case. The leftmost cases can shadow existing cases (i.e. the case
          which is used is the leftmost case for which the pattern matches
          successfully).}]

        TinyBang uses an approach which is very different from the one we
        followed in our Algebraic Data Types library, but contains the
        adequate primitives to build algebraic data types which would
        fulfill our requirements (aside from the ability to bound the set of
        extra ``row'' fields). We note that our flexible structs, which are
        used within the body of node-to-node functions in passes, do support
        an extension operation, which is similar to TinyBang's @${\&}, with
        the left-hand side containing a constant and fixed set of
        fields.@htodo{we probably can have / will have the ability to merge
          non-constant left-hand side values too.}}
      @item{Row polymorphism: Apparently, quoting a post on
        Quora@note{@hyperlink["https://www.quora.com/Object-Oriented-Programming-What-is-a-concise-definition-of-polymorphism"]{https://www.quora.com/Object-Oriented-Programming-What-is-a-concise-definition@|?-|-of-polymorphism}\label{quora-url-footnote}}:
        @aquote{
          Mostly only concatenative and functional languages (like Elm and PureScript) support this.
        }

        Classes in @typedracket can have a row type argument (but classes in
        @typedracket cannot have immutable fields (yet), and therefore
        occurrence typing does not work on class fields. Occurrence typing
        is an important idiom in @typedracket, used to achieve safe but
        concise pattern-matching, which is a feature frequently used when
        writing compilers).

        Our Algebraic Data Types library implements a bounded form of row
        polymorphism, and a separate implementation (used within the
        body of node-to-node functions in passes) allows unbounded row
        polymorphism.}
      @item{@todo{Virtual types}}
      @item{So-called ``untyped'' or ``uni-typed'' languages: naturally
        support most of the above, but without static checks.

        @htodo{
          @; phc/reading/CITE/Object-Oriented Programming_ What is a concise definition of polymorphism_ - Quora.html
          @; TODO: fix the footnote here!
          See also post on Quora@superscript{\ref{quora-url-footnote}},
          which links to @~cite{cardelli1985understanding}, and to a blog post by Sam
          Tobin-Hochstadt@note{@url|{https://medium.com/@samth/on-typed-untyped-and-uni-typed-languages-8a3b4bedf68c}|}
          The blog post by Sam Tobin-Hochstadt explains how @typedracket tries to
          explore and understand how programmers think about programs written in
          so-called ``untyped'' languages (namely that the programmers still
          conceptually understand the arguments, variables etc as having a type (or a
          union of several types)). @todo{Try to find a better citation for that.}}

        Operator overloading can be present in ``untyped'' languages, but is
        really an incarnation of single or multiple dispatch, based on the
        run-time, dynamic type (as there is no static type based on which the
        operation could be chosen). However it is not possible in ``untyped''
        languages and languages compiled with type erasure to dispatch on
        ``types'' with a non-empty intersection: it is impossible to
        distinguish the values, and they are not annotated statically with a
        type.

        As mentioned above, @typedracket does not have operator overloading,
        and since the inferred types cannot be accessed reflectively at
        compile-time, it is not really possible to construct it as a
        compile-time feature via macros. @typedracket also uses type erasure,
        so the same limitation as for untyped languages applies when
        implementing some form of single or multiple dispatch at run-time ---
        namely the intersection of the types must be empty. @todo{Here,
          mention (and explain in more detail later) our compile-time
          ``empty-intersection check'' feature (does not work with polymorphic
          variables).}}]

      @todo{Overview of the existing ``solutions'' to the expression problems, make
        a summary table of their tradeoffs (verbosity, weaknesses, strengths).}

      @todo{Compare the various sorts of subtyping and polymorphism in that light
        (possibly in the same table), even those which do not directly pose as a
        solution to the expression problem.}

      
      ``Nominal types'': our tagged structures and node types are not nominal types.

      The ``trivial'' Racket library tracks static information about the types in
      simple cases. The ``turnstile'' Racket language @todo{is a follow-up} work,
      and allows to define new typed Racket languages. It tracks the types of
      values, as they are assigned to variables or passed as arguments to functions
      or macros. These libraries could be used to implement operator overloads which
      are based on the static type of the arguments. It could also be used to
      implement unbounded row polymorphism in a way that does not cause a
      combinatorial explosion of the size of the expanded code.@todo{Have a look at
        the implementation of row polymorphism in @typedracket classes, cite their
        work if there is something already published about it.}

      From the literate program (tagged-structure-low-level):

      @quotation{
        Row polymorphism, also known as "static duck typing" is a type system
        feature which allows a single type variable to be used as a place
        holder for several omitted fields, along with their types. The
        @tt{phc-adt} library supports a limited form of row polymorphism:
        for most operations, a set of tuples of omitted field names must be
        specified, thereby indicating a bound on the row type variable.

        This is both a limitation of our implementation (to reduce the
        combinatorial explosion of possible input and output types), as well as a
        desirable feature.  Indeed, this library is intended to be used to write
        compilers, and a compiler pass should have precise knowledge of the
        intermediate representation it manipulates. It is possible that a compiler
        pass may operate on several similar intermediate representations (for
        example a full-blown representation for actual compilation and a minimal
        representation for testing purposes), which makes row polymorphism
        desirable. It is however risky to allow as an input to a compiler pass any
        data structure containing at least the minimum set of required fields:
        changes in the intermediate representation may add new fields which should,
        semantically, be handled by the compiler pass. A catch-all row type variable
        would simply ignore the extra fields, without raising an error. Thanks to
        the bound which specifies the possible tuples of omitted field names,
        changes to the the input type will raise a type error, bringing the
        programmer's attention to the issue. If the new type is legit, and does not
        warrant a modification of the pass, the fix is easy to implement: simply
        adding a new tuple of possibly omitted fields to the bound (or replacing an
        existing tuple) will allow the pass to work with the new type.  If, on the
        other hand, the pass needs to be modified, the type system will have
        successfully caught a potential issue.
      }
    }
  }@;{Algrbraic datatypes for compilers (phc-adt)}
