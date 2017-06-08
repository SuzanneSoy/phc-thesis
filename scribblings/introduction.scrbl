#lang scribble/manual

@require["util.rkt"
         (for-label racket
                    (only-in srfi/1 zip))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Introduction}

@asection{
 @atitle{The challenges of writing compilers}

 @epigraph[#:width "8cm"
           @elem{@italic{That Mitchell & Webb Look, Series 3} — BBC Two}]{
  Brain surgery? — It’s not exactly rocket science, is it?}

 Compilers are an essential part of today's software systems. Compilers
 translate high-level languages with complex semantics into lower-level
 languages. A compiler will parse the program, transform it in various ways,
 perform some more or less advanced static checks, and optimise the input
 program before producing an output in the desired target language. A compiler
 must be correct, extensible and fast: correct because programmers are
 concerned with logical errors in their own code, and should not fear that the
 compiler introduces erroneous behaviour on its own; extensible because the
 language is likely to evolve over time; and fast because the programmer's
 edit-build-test cycle should be as frequent as
 possible@todo{@~cite["smalltalk-programmer-efficiency-cycle"]}.

 Given their broad role, the complexity of the transformations involved, and
 the stringent requirements, writing compilers is a difficult task. Multiple
 pitfalls await the compiler engineer, which we will discuss in more detail
 below. This thesis aims to improve the compiler-writing toolkit currently
 available, in order to help compiler developers produce compilers which are
 closer to correctness, and easier to maintain.

 @require[scribble/core scribble/html-properties scribble/latex-properties]

 @hr

 The overall structure of a compiler will usually include a lexer and parser,
 which turn the the program's source into an in-memory representation. This
 initial representation will often be translated into an @deftech[#:key "IR"]{
  intermediate representation} (IR) better suited to the subsequent steps. At
 some early point, the program will be analysed for syntactical or semantic
 inconsistencies (ranging from missing parentheses to duplicate definitions of
 the same variable), and may also perform a more thorough static analysis. The
 translation can then include an optimisation phase, based on
 locally-recognisable patterns or on the results of the program-wide analysis
 performed separately. Finally, code in the target language or for the target
 architecture is generated.

 We identify three pitfalls which await the compiler-writer:

 @itemlist[
 @item{It is easy to reuse excessively a single @usetech{intermediate
    representation}, instead of properly distinguishing the features of the
   input and output of each pass;}
 @item{There is a high risk
   associated with the definition of large, monolithic passes, which are hard to
   test, debug, and extend;}
 @item{The fundamental structure of the program being compiled is often a
   graph, but compilers often work on an Abstract Syntax Tree, which requires
   explicit handling of the backward and transversal arcs; This is a source of
   bugs which could easily be avoided by using a higher-level abstraction
   specifically aiming to represent a graph.}]
 
 The two first issues are prone to manifestations of some form or another of
 the ``god object'' anti-pattern@note{The ``god object'' anti-pattern describes
  object-oriented classes which @emph{do} too much or @emph{know} too much. The
  size of these classes tends to grow out of control, and there is usually a
  tight coupling between the methods of the object, which in turn means that
  performing small changes may require understanding the interactions between
  random parts of a very large file, in order to avoid breaking existing
  functionality.}. The last issue is merely caused by the choice of an
 abstraction which does not accurately represent the domain. We will discuss
 each of these ailments in more detail in the following sections, and detail
 the undesirable symptoms associated with them.

 @asection{
  @atitle{Large monolithic passes}
   
  Large, monolithic passes, which perform many transformations in parallel have
  the advantage of possibly being faster than several smaller passes chained one
  after another. Furthermore, as one begins writing a compiler, it is tempting
  to incrementally extend an initial pass to perform more work, rather than
  starting all over again with a new @usetech{intermediate representation}, and
  a new scaffolding to support its traversal.

  However, the drawback is that large compiler passes are harder to test (as
  there are many more combinations of paths through the compiler's code to
  test), harder to debug (as many unrelated concerns interact to some extent
  with each other), and harder to extend (for example, adding a new special form
  to the language will necessitate changes to several transformations, but if
  these are mingled in a single pass, the changes may be scattered through it,
  and interact with a significant amount of surrounding code). This higher
  maintenance cost also comes with another drawback: formal verification of the
  compiler will clearly be more difficult when large, tangled chunks of code
  which handle different semantic aspects are involved.

  @todo{Talk a bit about compcert here (one of the few/ the only formally
   verified compilers).}

 }

 @asection{
  @atitle{Overusing a single @usetech{intermediate representation}}

  The static analysis, optimisation and code generation phases could in
  principle work on the same @usetech{intermediate representation}. Several
  issues arise from this situation, however.

  In principle, new information gained by the static analysis may be added to
  the existing representation via mutation, or the optimiser could directly
  alter the @tech{ IR}. This means that the @tech{IR} will initially contain
  holes (e.g. represented by @racketid[null] values), which will get filled in
  gradually. Manipulating these parts is then risky, as it is easy to
  accidentally attempt to retrieve a value before it was actually computed.
  Using the same @tech{IR} throughout the compiler also makes it difficult for
  later passes to assume that some constructions have been eliminated by
  previous simplification passes, and correctness relies on a fixed order of
  execution of the passes; parts of the code which access data introduced or
  modified by other passes are more brittle and may be disrupted when code is
  refactored (for example, when moving the computation of some information to a
  later pass).

  This situation becomes worse during the maintenance phase of the compiler's
  lifecycle: when considering the data manipulated by a small portion of code
  (in order to fix or improve said code), it is unclear which parts are supposed
  to be filled in at that point, as well as which parts have been eliminated by
  prior simplification passes.

  Furthermore, a mutable @tech{IR} hinders parallel execution of compiler
  passes. Indeed, some compiler passes will perform global transformations or
  transversal analyses, and such code may be intrinsically difficult to @htodo{
   parallelise}. @;{is "parallelise" the right word?} Many other passes however
  are mere local transformations, and can readily be executed on distinct parts
  of the abstract syntax tree, as long as there is no need to synchronise
  concurrent accesses or modifications.
  
  Using immutable intermediate representations (and performing shallow copies
  when updating data) can help with this second issue. However, there is more to
  gain if, instead of having many instances of the same type, each intermediate
  representation is given a distinct, precise type. The presence or absence of
  computed information can be known from the input and output type of a pass,
  instead of relying on the order of execution of the passes in order to know
  what the input data structure may contain.

 }

 @asection{
  @atitle{Graphs}

   @htodo{full stop in the middle of sentence}
  Nontrivial programs are inherently graphs: they may contain mutually
  recursive functions (which directly refer to each other, and therefore will
  form a cycle in a representation of the program), circular and (possibly
  mutually) recursive datatypes may syntactically contain (possibly indirect)
  references to themselves, and the control flow graph of a function or method
  can, as its name implies, contain instructions which perform conditional or
  unconditional backwards branches.

  However, nearly every compiler textbook will mention the use of
  @tech[#:key "AST"]{Abstract Syntax Trees} (ASTs) to represent the program.
  This means that a structure, which intrinsically has the shape of a graph, is
  encoded as a tree.

  Edges in the graph which may embody backward references can be made explicit
  in various ways:

  @itemlist[
 @item{By using a form of unique identifier like a name bearing some semantic
    value (e.g. the fully qualified name of the type or function that is
    referred to), an index into an array of nodes (e.g. the offset of an
    instruction in a function's bytecode may be used to refer to it in the
    control flow graph), an automatically-generated unique identifier.

    Manipulation of these identifiers introduces a potential for some sorts of
    bugs: name clashes can occur if the chosen qualification is not sufficient
    to always distinguish nodes. Thus compiler passes which
    duplicate nodes (for example specialising functions) or merge them must be
    careful to correctly update identifiers.

    Anecdotally, we can mention a bug in the @|monocecil| library (which allows
    easy manipulation of @|dotnet| bytecode). When ``resolving'' a reference to
    a primitive type, it can happen in some cases that @|monocecil| returns a
    @tt{Type} metadata object which references a type with the correct name, but
    @todo{exported} from the wrong @|DLL| library.}
 @item{Alternatively, backward references may be encoded as a form of path
    from the referring node. @DeBruijn indices can be used in such an encoding,
    for example.

    Once again, manipulating these references is risky, and @DeBruijn indices
    are particularly brittle, for example when adding a wrapper around a node
    (i.e. adding an intermediate node on the path from the root), the @DeBruijn
    indices used in some of the descendents of that node (but not all) must be
    updated. It is understandably easy to incorrectly implement updates of these
    indices, and a single off-by-one error can throw the graph's representation
    into an inconsistent state.}
 @item{The program's representation could also contain actual pointers
    (thereby really representing the program as an ``Abstract Syntax Graph''),
    using mutation to patch nodes after they are initially created.

    In order to prevent undesired mutation of the graph after it is created, it
    is possible to ``freeze'' the objects contained within@todo{references}.
    This intuitively gives guarantees similar to those obtained from a
    purely immutable representation. However, the use of mutation could obstruct
    formal verification efforts, as some invariants will need to take into
    account the two phases of an object's lifetime (during the creation of the
    containing graph, and after freezing it). More generally speaking, it is
    necessary to ensure that no mutation of objects happens during the graph
    construction, with the exception of the mutations required to patch cycles.}
 @item{The compiler could also manipulate lazy data structures, where the
    actual value of a node in the graph is computed on the fly when that node is
    accessed.

    Lazy programs are however harder to
    debug@~cite["nilsson1993lazy" "wadler1998functional" "morris1982real"],
    as the computation of the various parts of the data manipulated does not
    occur in an intuitive order. Among other things, accidental infinite
    recursion could be triggered by totally unrelated code which merely reads a
    value.}
 @item{Finally, Higher-Order Abstract Syntax, or @|HOAS| for short, is a
    technique which encodes variable bindings as anonymous functions in the host
    language (whose parameters reify bindings at the level of the host
    language). Variable references are then nothing more than actual uses of the
    variable at the host language's level. Substitution, a common operation in
    compilers and interpreters, then becomes a simple matter of calling the
    anonymous function with the desired substitute. @|HOAS| has the additional
    advantage that it enforces well-scopedness, as it is impossible to refer to
    a variable outside of its scope in the host language.

    Parametric @|HOAS|, dubbed @|PHOAS|, also allows encoding the type of the
    variables in the representation. @todo{Can extra information other than the
     type be stored?}

    There are a few drawbacks with @|HOAS| and @|PHOAS|:

    The ``target'' of a backward reference must be above all uses in the tree
    (i.e. a node may be the target of either backward references, forward
    references, but not a mix of both). This might not always be the case. For
    example, pre/post-conditions could, in an early pass in the compiler, be
    located outside of the normal scope of a function's signature, but still
    refer to the function's parameters. If the pre/post-condition language
    allows breaking encapsulation, these could even refer to some temporary
    variables declared inside the function.

    @;{
     @; True for HOAS, not sure for PHOAS.
     @todo{The ``target'' of a backward reference does not initially contain
      additional data (e.g. the variable name to be used for error messages, its
      static or concrete type and so on) although extending the encoding to
      support this should be feasible.}
    }

    @todo{@|PHOAS| naturally lends itself to the implementation of substitutions,
     and therefore is well-suited to the writing of interpreters. However, the
     representation cannot be readily traversed and accessed as would be done
     with normal structures, and therefore the model could be
     counterintuitive.}

    @todo{It seems difficult to encode an arbitrary number of variables bound in
     a single construct (e.g. to represent bound type names across the whole
     program, or an arbitrary number of mutually-recursive functions declared
     via @racketid[let … and … in …], with any number of @racketid[and] clauses
     in the compiled language.}}]
  
  Although some of these seem like viable solutions (e.g. explicitly freezing
  objects), they still involve low-level mechanisms to create the graph. When
  functionally replacing a node with a new version of itself, all references to
  it must be updated to point to the new version. Furthermore, code traversing
  the graph to gather information or perform updates needs to deal with
  cycles, in order to avoid running into an infinite loop (or infinite
  recursion). Finally, backward edges are not represented in the same way as
  forward edges, introducing an arbitrary distinction when fetching data from
  the neighbours of a node. This last aspect reduces the extensibility of code
  which manipulates graphs where access to fields is not done uniformly:
  supposing new features of the language to be compiled require turning a
  ``seamless'' edge into one which must be explicitly resolved in some way (e.g.
  because this node, in the updated @tech{IR}, may now be part of cycles), this
  change of interface will likely break old code which relied on seamless access
  to that field.

  We think that the compiler engineer could benefit from abstracting over these
  implementation details, and think of compiler passes in terms of graph
  transformations. Programmers using functional languages often write list
  transformations using @htodo{higher-order} functions like @racket[map],
  @racket[foldl], @racket[filter], @racket[zip] and so on, instead of explicitly
  writing recursive functions.

  The graph can be manipulated by updating some or all nodes of a given type,
  using an old-node to new-node transformation function. This transformation
  function could produce more than one node, by referencing the extra nodes from
  the replacement one. It should furthermore be possible to locally navigate
  through the graph, from its root and from the node currently being
  transformed. This interface would allow to seamlessly handle cycles —
  transformations which apply over a whole collection of nodes need not be
  concerned with cycles — and still allow local navigation, without
  distinguishing backward and forward edges.

  @htodo{Be a bit more verbose on what cool stuff it allows}

  @htodo{Think about ensuring that nodes from two distinct graphs are not mixed
   in unexpected ways (placing a dummy phantom type somewhere should be enough
   to prevent it).}
 
 }

 @asection{
  @atitle{Expressing the data dependencies of a path via row types}

  It is easy enough to test a compiler by feeding it sample programs and
  checking that the compiled output behaves as expected. However, @htodo{
   triggering} a specific set of conditions inside a given pass, in order to
  achieve reasonably complete code coverage, may prove to be a harder task:
  previous passes may modify the input program in unexpected ways, and obtaining
  a certain data configuration at some point in the compiler requires the
  developer to mentally execute the compiler's code @emph{backwards} from that
  point, in order to determine the initial conditions which will produce the
  desired configuration many steps later. This means that extensively testing
  corner cases which may occur in principle, but are the result of unlikely
  combinations of features in the input program, is a cumbersome task. @htodo{
   i.e. unit testing vs other forms which test larger components.}

  If the compiler consists of many small passes, whose inputs and outputs are
  serializable, then it becomes possible to thoroughly test a single pass in
  isolation, by supplying an artificial, crafted input, and checking some
  properties of its output.

  However, a compiler written following the @|nanopass| philosophy will feature
  many small passes which read and update only a @htodo{small: repetition} small
  part of their input. Specifying actual values for the irrelevant parts of the
  data not only makes the test cases more verbose than they need to be, but also
  hides out of plain sight which variations of the input matter (and may thus
  allow the detection of new errors), and which parts of the input are mere
  placeholders whose actual value will not influence the outcome of the pass,
  aside from being copied over without changes.

  It is desirable to express, in a statically verifiable way, which parts of
  the input are relevant, and which parts are copied verbatim (modulo updated
  sub-elements). Furtherfomre, it would be useful to be able to only specify the
  relevant parts of tests, and omit the rest (instead of supplying dummy
  values).

  @htodo{embed within each graph a ``mapper'': specify one (or more?) mappings
   (not anchored to the root) and the mapper updates these nodes, keeping the
   rest intact. A pass then may expect a ``mappable'' object, regardless of the
   actual shape of the irrelevant parts (including those on the paths between
   the root and the relevant nodes).}

  Row polymorphism allows us to satisfy both expectations. The irrelevant
  fields of a record and the irrelevant cases of a variant type can be
  abstracted away under a single row type variable. ``Row'' operations on
  records allow reading and updating relevant fields, while keeping the part
  abstracted by the row type variable intact. When invoking a compiler pass, the
  row type variables may be instantiated to the full set of extra fields present
  in the real @tech{IR}, when the pass is called as part of the actual
  compilation; it is also possible, when the pass is called during testing, to
  instantiate them to an empty set of fields (or to use a single field
  containing a unique identifier, used to track ``object identity'').}

 @asection{
  @atitle{Verification}

  The implementation presented in this thesis cannot be immediately extended to
  support end-to-end }
 

 @;{
  The static analysis, optimisation and code generation phases will often work
  on that @usetech{intermediate representation}.

  These transformations are often non-trivial and may require aggregating and
  analysing data scattered across the program.

  We build upon the achievements of the @|nanopass-c-f| project,
  which is presented in more detail in section XYZ. Simply put, @|nanopass| helps
  the programmer define a myriad of compiler passes, each doing a very small
  amount of code (and therefore easy to test and maintain), and each with a
  different input and output type.
  
 }
}

@asection{
 @atitle{Summary}
 
 @asection{ @atitle{Extensible type system} We implemented a different type
  system on top of @|typedracket|, using macros. Macros have been used not only
  to extend a language's syntax (control structures, contract annotations, and
  so on), but also to reduce the amount of ``boilerplate'' code and obtain
  clearer syntax for common or occasional tasks. Macros have further been used
  to extend the language with new paradigms, like adding an object system
  (CLOS@~cite["bobrow_common_1988"]@todo{is it really implemented using
   macros?}, Racket classes@~cite["flatt_scheme_classes_2006"]) or supporting
  logic programming (Racket
  Datalog@note{@url{http://docs.racket-lang.org/datalog/}} and
  Racklog@note{@url{http://docs.racket-lang.org/racklog/}},
  Rosette@~cite["torlak_growing_rosette_2013"]). In the past,
  @|typedracket|@~cite["tobin-hochstadt_design_2008"] has proved that a type
  system can be successfully fitted onto an existing ``untyped'' language, using
  macros. We implemented the @racketmodname[type-expander] library atop
  @|typedracket|, without altering @|typedracket| itself. Our
  @racketmodname[type-expander] library allows macros to be used in contexts
  where a type is expected.

  This shows that an existing type system can be made extensible using macros,
  without altering the core implementation of the type system. We further use
  these type expanders to build new kinds of types which were not initially
  supported by @|typedracket|: non-nominal algebraic types, with row
  polymorphism. @|Typedracket| has successfully been extended in the past (for
  example by adding type primitives for Racket's class system, which
  incidentally also support row polymorphism), but these extensions required
  modifications to the trusted core of @|typedracket|. Aside from a small hack
  (needed to obtain non-nominal algebraic types which remain equivalent across
  multiple files), our extension integrates seamlessly with other built-in
  types, and code using these types can use idiomatic @|typedracket| features.

  @Typedracket was not initially designed with this extension in mind, nor,
  that we know of, was it designed with the goal of being extensible. We
  therefore argue that a better choice of primitive types supported by the
  core implementation could enable many extensions without the need to resort
  to hacks the like of which was needed in our case. In other words, a better
  design of the core types with extensibility in mind would have made our job
  easier.

  In particular, Types in Typed
  Clojure@~cite["practical_types_for_clojure_2016"] support fine-grained typing
  of heterogeneous hash tables, this would likely allow us to build much more
  easily the ``strong duck typing'' primitives on which our algebraic data types
  are based, and without the need to resort to hacks.

  In languages making heavy uses of macros, it would be beneficial to design
  type systems with a well-chosen set of primitive type, on top of which more
  complex types can be built using macros.

  Building the type system via macros atop a small kernel is an approach that
  has been pursued by Cur, a dependently-typed language developed with Racket,
  in which the tactics language is entirely built using macros, and does not
  depend on Cur's trusted type-checking core.}

 @asection{
  @atitle{Compiler-writing framework}

  Our goal was to introduce a compiler-writing framework, which:
  @itemlist[
 @item{Supports writing a compiler using many small passes (in the spirit of
    @|nanopass|)}
 @item{Allows writing the compiler in a strongly-typed language}
 @item{Uses immutable data structures for the Intermediate Representations
    (ASTs)}
 @item{Supports transversal and backwards branches in the AST, making it
    rather an Abstract Syntax Graph (this is challenging due to the use of
    immutable data structures).}
 @item{Provides easy manipulation of the Intermediate Representations: local
    navigation from node to node, global higher-order operations over many
    nodes, easy construction, easy serialization, with the guarantee that at
    no point an incomplete representation can be manipulated. These operations
    should handle seamlessly transversal and backwards arcs.}
 @item{Enforces structural invariants (either at compile-time or at
    run-time), and ensures via the type system that unchecked values cannot be
    used where a value respecting the invariant is expected.}
 @item{Has extensive support for testing the compiler, by allowing the
    generation of random ASTs @todo{(note that the user guides the random
     generation, it's not fully automatic like quickcheck)}, making it
    possible to read and write ASTs from files and compare them, and allows
    compiler passes to consume ASTs containing only the relevant fields (using
    row polymorphism).}]}

 @htodo{
  testing (random stuff)

  TODO:@~cite{quickcheck} and other things related to test generation
  (I have something in Zotero)

  TODO: and the l-sets (find something about that)

  TODO: Problems with ``mocks'' and strong typing (null values everywhere,
  bleargh).
 }

}

