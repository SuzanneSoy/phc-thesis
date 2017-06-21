#lang scribble/manual

@require["util.rkt"
         scriblib/render-cond
         (for-label racket
                    (only-in srfi/1 zip))]
@(use-mathjax)

@htodo{No parsers}

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Introduction}

@asection{
 @atitle{Warm-up}

 @todo{What does a compiler do}

 @todo{This thesis aims to build a framework which helps write compilers.}
 
 @todo{Our focus is on compilers, not virtual machines or other run-time
  systems. We are also not concerned with parsers — there are lots of existing
  approaches and libraries which help writing parsers, although parsing in
  general is not yet a solved problem on all accounts.}
 
 @todo{IR = the macroscopic structure of the program (i.e. the meta-model
  (explain what it is)) + the code of functions and/or methods (statements and
  expressions, basic blocks of statements, or bytecode instructions)}}

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
 must be correct, reusable and fast. It must be correct because programmers are
 concerned with logical errors in their own code, and should not fear that the
 compiler introduces erroneous behaviour on its own. It must be also
 well-architectured: extensible, because the language is likely to evolve over
 time, modular in the hope that some components can be improved independently
 of the rest of the compiler (e.g. replacing or improving an optimisation
 phase, or changing the compiler's front-end, to support another input
 language), and more generally reusable, so that parts can be repurposed to
 build other compilers, or other tools (analysers, IDEs, code instrumentation
 and so on). Finally, a fast compiler is desirable because the programmer's
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
 which turn the program's source into an in-memory representation. This initial
 representation will often be translated into an @deftech[#:key "IR"]{
  intermediate representation} (IR) better suited to the subsequent steps. At
 some early point, the program will be analysed for syntactical or semantic
 inconsistencies (ranging from missing parentheses to duplicate definitions of
 the same variable), and may also perform a more thorough static analysis.
 Finally, code in the target language or for the target architecture is
 generated. The translation can additionally include optimisation phases in
 several spots: during code generation, using locally-recognisable patterns, or
 for example earlier, using the results of the program-wide analysis performed
 separately.

 We identify three pitfalls which await the compiler-writer:

 @itemlist[
 @item{It is easy to reuse excessively a single @usetech{intermediate
    representation} type, instead of properly distinguishing the specifics of
   the input and output type of each pass;}
 @item{There is a high risk
   associated with the definition of large, monolithic passes, which are hard to
   test, debug, and extend;}
 @item{The fundamental structure of the program being compiled is often a
   graph, but compilers often work on an Abstract Syntax Tree, which requires
   explicit handling of the backward arcs; This is a source of bugs which could
   be avoided by using a better abstraction.}]
 
 The first two issues are prone to manifestations of some form or another of
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
   
  Large, monolithic passes, which perform many transformations simultaneously
  have the advantage of possibly being faster than several smaller passes
  chained one after another. Furthermore, as one begins writing a compiler, it
  is tempting to incrementally extend an initial pass to perform more work,
  rather than starting all over again with a new @usetech{intermediate
   representation}, and a new scaffolding to support its traversal.

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
  principle work on the same @usetech{intermediate representation}. However,
  several issues arise from this situation.

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
  analyses, and such code may be intrinsically difficult to 
  parallelise. @htodo{is "parallelise" the right word?} Many other passes
  however are mere local transformations, and can readily be executed on
  distinct parts of the abstract syntax tree, as long as there is no need to
  synchronise concurrent accesses or modifications.
  
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
  @atitle{Expressing the data dependencies of a pass via row types}

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
  sub-elements). Furthermore, it would be useful to be able to only specify the
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

  @todo{Needs a transition from the previous section, or this should be moved
   elsewhere.}

  The implementation presented in this thesis cannot be immediately extended to
  support end-to-end formal verification of the compiler being written. However,
  it contributes to pave the way for writing formally verified compilers:
  firstly, the smaller passes are easier to verify. Secondly, the use of
  intermediate representations which closely match the input and output data can
  be used, given a formal semantic of each @tech{IR}, to assert that a
  transformation pass is systematically preserving the semantics of the input.
  Thirdly, the use of a typed language instead of the currently ``untyped''
  @|nanopass| framework means that a lot of properties can be ensured by relying
  on the type system. @|Typedracket|'s type checker is not formally verified
  itself and would have to be trusted (alternatively, the adventurous researcher
  could attempt to derive well-typedness proofs automatically by hijacking the
  type checker to generate traces of the @htodo{type checking} steps involved,
  or manually, only using the type checker as a helper tool to detect and weed
  out issues during development. Fourthly, the explicit specification of the
  dependencies of passes on their input via row types is a form of frame
  specification@htodo{reference}, and can significantly ease the verification
  effort, as the engineer can rely on the fact that irrelevant parts were not
  modified in the output. These @htodo{frame properties}@htodo{or rather frame
   specifications?} are statically enforced by our extension of @|typedracket|'s
  type system, which we formalise in chapter @todo{??}. This relies on trusting
  @|typedracket| itself once again, and on the correctness of the implementation
  of our translation from the extended type system to @|typedracket|'s core type
  system. Fifthly, we provide means to express graph transformations as such
  instead of working with an encoding of graphs as abstract syntax trees (or
  directed acyclic graphs), with explicit backward references.
  We are hopeful that eliminating this mismatch will be beneficial to the formal
  verification of the transformation passes.

  These advantages would be directly available to engineers attempting a formal
  proof of their compiler, while trusting the correctness of @|typedracket|, as
  well as that of our framework. The implementation of our framework is not
  hardened, and @|typedracket| itself suffers from a small number of known
  sources of unsoundness@note{See
   @url["https://github.com/racket/typed-racket/issues"].}, however. In order to
  do an end-to-end verification of a compiler, it would be necessary to port our
  approach to a language better suited to formal verification. Alternatively,
  Racket could in principle be extended to help formalisation efforts. Two
  approaches come to mind: the first consists in proving that the macros
  correctly implement the abstraction which they attempt to model; the second
  would be to have the macros inject annotations and hints indicating properties
  that must be proven, in the same way that type annotations are currently
  inserted. These hints could then be used by the prover to generate proof
  obligations, which could then be solved manually or automatically.

  @htodo{atitle{…}}

  Mixing macros and language features which help obtaining static guarantees is
  a trending topic in the Racket ecosystem and in other communities.

  @|Typedracket| is still in active development, and several other projects
  were presented recently.

  @|Hackett|@note{@url["https://github.com/lexi-lambda/hackett"]}, mainly
  developed by @lastname{King}, is a recent effort to bring a
  @|haskell98|-like type system to Racket.

  @|Hackett| is built on the techniques developed by @lastname{Chang},
  @lastname{ Knauth} and @lastname{Greenman} in
  @~cite["chang2017type-systems-as-macros"], which lead to the @|turnstile|
  library@note{@url["https://bitbucket.org/stchang/macrotypes.git"]}.
  @|Turnstile| is a helper library which facilitates the creation of typed
  languages in Racket. Macros are amended with typing rules, which are used to
  thread type information through uses of macros, definition forms and other
  special forms. Type checking is therefore performed during macro-expansion,
  and does not rely on an external type checker which would work on the expanded
  code. As a result, new languages built with Racket and @|turnstile| are not
  limited to a pre-existing type system, but can rather devise their own from
  the ground up. This approach brings a lot of flexibility, the drawback being
  that more trust is put in the language designer.

  The work presented in this thesis@htodo{, which started before @|turnstile|
   was publicly announced,} aims to follow a different path than that followed
  by @|turnstile|: we chose to implement our extended type system as an
  abstraction over the existing type system of @|typedracket|. This means that
  we do not rely so much on the correctness of our typing rules: instead of
  verifying ourselves the well-typedness of compilers written using our
  framework, we inject type annotations in the expanded code, which are then
  verified by @|typedracket|. Therefore, we are confident that type errors will
  be caught by @|typedracket|, safe in the knowledge that the code obtained
  after macro-expansion is type-safe@note{We actually use on a few occasions
   @racket[unsafe-cast]. Such a blunt instrument is however only used in cases
   where @|typedracket| already has the required type information, but the type
   checker fails to deduce the equivalence between two formulations of the same
   type, or does not acknowledge that the term being checked has the expected
   type. These issues are being worked on by the developers of @|typedracket|,
   however, and we hope to be able to remove these uses of @racket[unsafe-cast]
   in later versions.}. This increased serenity comes at the cost of
  flexibility, as @|typedracket|'s type system was not able to express the type
  constructs that we wanted to add. We therefore had to resort to a few hacks to
  translate our types into constructs that could be expressed using
  @|typedracket|.

  The approach of building more complex type constructs atop a small trusted
  kernel has been pursued by
  @|cur|@note{@url["https://github.com/wilbowma/cur"]}, developed by @lastname{
   Bowman}. @|Cur| is a dependently typed language which permits theorem proving
  and verified programming. It is based on a small kernel (the Curnel), which
  does not contain language features which can be expressed by macro
  transformations. Most notably, the prover's tactics are defined using
  metaprogramming tools, and are not part of the core language.

  Another tool worth mentioning is
  Rosette@note{@url["https://github.com/emina/rosette"]}@todo{reference}.
  Rosette, mainly developed by @lastname{Torlak}, is a solver-aided language: it
  tightly integrates an SMT solver with some Racket constructs, so that powerful
  queries can be performed and answered, for example ``what input values to the
  function f will generate outputs which satisfy the predicate p?''. It can also
  generate simple functions which satisfy given conditions. These features allow
  it to be used both as a helper tool during development, for engineers coming
  from various domains, and as a verifier, as the solver can be used to assert
  that a function will never give an erroneous output, given a set of
  constraints on its input.

  @htodo{Check that I'm not saying anything wrong about Turnstile here.} The
  idea of expressing the type of macro transformations at some level (e.g. by
  indicating the type of the resulting expression in terms of the type of the
  input terms, as is allowed by @|turnstile|) is not new: in 2001, Ganz, Sabry
  and Taha already presented in 2001
  MacroML@~cite["ganz2001macros-as-multi-stage-computations"], an experimental
  language which allows type checking programs before macro-expansion. However,
  it seems that there is interest, both in the Racket community and elsewhere,
  for languages with powerful metaprogramming facilities, coupled with an
  expressive type system. We are hopeful that this will lead to innovations
  concerning the formal verification of programs making heavy use of complex
  macros.
 }
 

 @;{
  The static analysis, optimisation and code generation phases will often work
  on that @usetech{intermediate representation}.

  These transformations are often non-trivial and may require aggregating and
  analysing data scattered across the program.

  We build upon the achievements of the @|nanopass-c-f| project,
  which is presented in more detail in section XYZ. Simply put, @|nanopass|
  helps the programmer define a myriad of compiler passes, each doing a very
  small amount of code (and therefore easy to test and maintain), and each with
  a different input and output type.
  
 }
}

@asection{
 @atitle{Summary}

 @epigraph[#:width "8cm"
           @elem{Unknown}]{
  Once upon a time…}
 
 @asection{ @atitle{Extensible type system} We implemented a different type
  system on top of @|typedracket|, using macros. Macros have been used not only
  to extend a language's syntax (control structures, contract annotations, and
  so on), but also to reduce the amount of ``boilerplate'' code and obtain
  clearer syntax for common or occasional tasks. Macros have further been used
  to extend the language with new paradigms, like adding an object system
  (CLOS@~cite["bobrow_common_1988"]@todo{is it really implemented using
   macros?}, Racket classes@~cite["flatt_scheme_classes_2006"]) or supporting
  logic programming (Racket
  @cond-element[
 [html @elem{
      @seclink["top" #:doc '(lib "datalog/scribblings/datalog.scrbl")]{Datalog}
      and
      @seclink["top" #:doc '(lib "racklog/racklog.scrbl")]{Racklog}}]
 [else @elem{
      Datalog@note{@url{http://docs.racket-lang.org/datalog/}} and
      Racklog@note{@url{http://docs.racket-lang.org/racklog/}}}]],
  Rosette@~cite["torlak_growing_rosette_2013" "torlak_rosette_symbolic_vm"]). In
  the past, @|typedracket|@~cite["tobin-hochstadt_design_2008"] has proved that
  a type system can be successfully fitted onto an existing ``untyped''
  language, using macros. We implemented the @racketmodname[type-expander]
  library atop @|typedracket|, without altering @|typedracket| itself. Our
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
  type systems with a well-chosen set of primitive types, on top of which more
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
 @item{Supports backwards branches in the AST, making it
    rather an Abstract Syntax Graph (this is challenging due to the use of
    immutable data structures).}
 @item{Provides easy manipulation of the Intermediate Representations: local
    navigation from node to node, global higher-order operations over many
    nodes, easy construction, easy serialization, with the guarantee that at
    no point an incomplete representation can be manipulated. These operations
    should handle seamlessly backwards arcs.}
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

 @asection{
  @atitle{Overview}

  The rest of this document is structured as follows:

  @htodo{auto chapter and section numbers}
  @(let ([related-chap
          @seclink["State_of_the_art"
                   #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")
                   ]{Chapter 2}]
         [related-type-expander
          @seclink["related-type-expander"
                   #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")
                   ]{Section 2.1}]
         [related-adt
          @seclink["related-adt"
                   #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")
                   ]{Section 2.2}]
         [related-nanopass
          @seclink["related-nanopass"
                   #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")
                   ]{section 2.3}]
         [related-cycles
          @seclink["related-cycles"
                   #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")
                   ]{section 2.4}]
         [initial-examples-chap
          @seclink["initial-examples-chap"
                   #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")
                   ]{Chapter 3}]
         [tr-chap
          @seclink["tr-chap"
                   #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")
                   ]{Section 4.1}]
         [type-expander-chap
          @seclink["type-expander-chap"
                   #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")
                   ]{Section 4.2}]
         [adt-chap
          @seclink["adt-chap"
                   #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")
                   ]{Section 4.3}]
         [typed-nanotrees-chap
          @seclink["typed-nanotrees-chap"
                   #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")
                   ]{Section 5.1}]
         [typed-nanodags-chap
          @seclink["typed-nanodags-chap"
                   #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")
                   ]{Section 5.2}]
         [typed-nanographs-chap
          @seclink["typed-nanographs-chap"
                   #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")
                   ]{Section 5.3}]
         [structural-invariants-chap
          @seclink["structural-invariants-chap"
                   #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")
                   ]{Section 5.4}]
         [future-extensions-chap
          @seclink["future-extensions-chap"
                   #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")
                   ]{Section 5.5}]
         [results-chap
          @seclink["results-chap"
                   #:doc '(lib "phc-thesis/scribblings/phc-thesis.scrbl")
                   ]{Chapter 6}])
     @list{
   @itemlist[
 @item{@|related-chap| presents related work. It discusses approaches to
     extensible type systems in @|related-type-expander|. @|related-adt|
     considers how structures, variants and polymorphism may exhibit different
     properties in different languages, and makes the case for bounded row
     polymorphism as a well-suited tool for building compilers.
     The @|nanopass-c-f| is presented in @|related-nanopass|, and techniques
     used to encode and manipulate graphs are studied in @|related-cycles|.}
 @item{In @|initial-examples-chap|, we give some example uses of the compiler
     framework described in this thesis. We indicate the pitfalls, bugs
     and boilerplate avoided thanks to its use.
     @todo{Move this above the related work?}}
 @item{@|tr-chap| gives an overview of the features of @|typedracket|'s type
     system. It then formalises the features relevant to our work, by giving
     the subtyping rules, as well as the builder and accessor primitives
     for values of these types.}
 @item{@|type-expander-chap| explains how we implemented support for type-level
     macros as an extension to @|typedracket|, without modifying the core
     implementation. We detail the reduction rules which allow translating
     a type making use of expanders into a plain type.}
 @item{@|adt-chap| discusses the flavour of algebraic datatypes which we chose
     to implement atop @|typedracket|, as well as its extension with row types.
     We explain in detail our goals and constraints, and present two
     implementations — a first attempt, which unfortunately required verbose
     annotations for some ``row operations'', and a second solution, which
     greatly reduces the number of annotations required, by using a different
     implementation strategy. We give formal semantics for these extensions,
     give the reduction rules which translate them back to @|typedracket|'s type
     system, and show that this reduction preserves the original semantics. We
     then finally define a layer of syntactic sugar which allows convenient
     use of these new type primitives.}
 @item{@|typed-nanotrees-chap| builds upon these algebraic datatypes and row
     types to define a typed version of the @|nanopass-c-f| which operates on
     abstract syntax trees. We explain the notions of
     @htodo{tech}@tech{tree nodes}, @htodo{tech}@tech{mapping functions} and
     @htodo{tech}@tech{cata-morphisms}, show how these interact with row typing,
     and give an overview of the implementation. We then extend this with
     @tech{detached mappings}: this feature allows the user to map a function
     over all nodes of a given type in a graph, regardless of the structure of
     the graph outisde of the relevant parts which are manipulated by the
     mapping function. This allows test data to not only omit irrelevant
     branches in the abstract syntax tree by omitting the field pointing to
     these branches, but also irrelevant parts located above the interesting
     nodes. In other words, this feature allows cutting and removing the top of
     the abstract syntax tree, and glue together the resulting forest.
     We also formally describe the result of applying a set of detached or
     regular mapping functions to an input tree.}
 @item{@|typed-nanodags-chap| extends this typed version of @|nanopass| to
     handle directed acyclic graphs. We start by considering concerns such as
     equality of nodes (for which the previous chapter assumed a predicate
     existed, without actually implementing it), and hash consing. This allows
     us to prepare the ground for the extension presented in the next chapter,
     namely graphs.}
 @item{We can then introduce support for cycles in @|typed-nanographs-chap|:
     instead of describing abstract syntax tree transformations, it then becomes
     possible to describe graphs transformations. To this end, we introduce new
     kinds of nodes: @tech{placeholders}, which are used during the construction
     of the graph, @tech{with-indices} nodes, which encode references to
     neighbouring nodes as indices into arrays containing all nodes of a given
     type, for the current graph, and @tech{with-promises} nodes, which hide
     away this implementation detail by lazily resolving all references, using
     a uniform API. This allows all fields of a given node to be accessed in the
     same way, while still allowing logical cycles built atop a purely immutable
     representation.

     We give an overview of how our implementation handles cycles, using
     worklists which gather and return @tech{placeholders} when the mapping
     functions perform recursive calls, and subsequently turn the results into
     into @tech{with-indices} nodes, then into @tech{with-promises} ones.
     We then update our notion of equality and hash consing to account for
     cycles, and update the formal semantics too.}
 @item{@;{The fourth feature…}
     Extra safety can be obtained by introducing some structural invariants
     which constrain the shape of the graph. For example, it is possible to
     ensure the well-scopedness of variable references. Another desirable
     property would be the absence of cycles, either to better model the
     @tech{IR}, knowing that cyclic references are not allowed at some point by
     the target language, or to detect early on changes in the @tech{IR} which
     may break code assuming the absence of cycles. A third option would be to
     ensure that ``parent'' pointers are correct, and that the node containing
     them is indeed referenced by the parent (i.e., ensure the @emph{presence}
     of well-formed cycles). @|structural-invariants-chap| presents an extension
     of our graph manipulation framework, which allows the specification of
     structural invariants. These can in some cases be checked statically,
     in other cases it may be necessary to follow a macro-enforced discipline,
     and as a last resort, a dynamic check may be performed.
     
     @htodo{+ singleton / all nodes of a given type referenced by a central
      point with bounded length / only one node with a given value for a given
      property.}
    
     We further explain how we use phantom types to reflect at the type level
     which invariants were checked on an instance of a graph. The types used to
     represent that an instance satisfies an invariant are chosen so that
     instances with stronger invariants are subtypes of instances with weaker
     invariants.}
 @item{Finally, in @|future-extensions-chap| we succinctly present some
     extensions which could be added to the framework presented in the previous
     chapters. We discuss how it would be possible to garbage-collect unused
     parts of the graph when only a reference to an internal node is kept, and
     the root is logically unreachable. Another useful feature would be the
     ability to define general-purpose graph algorithms (depth-first traversal,
     topological sort, graph colouring, and so on), operating on a subset of
     the graph's fields. This would allow to perform these common operations
     while considering only a subgraph of the one being manipulated. Finally,
     we mention the possibility to implement an α-equivalence comparison
     operator.@;{α-normal form}}
 @item{In @|results-chap|, we present more examples and revisit the initial
     examples illustrating our goals in the light of the previous chapters.
     
     We ported the most complete compiler written with @|nanopass| (a Scheme
     compiler) to our framework; we summarise our experience and compare our
     approach with @|nanopass|, by indicating the changes required, in
     particular how many additional type annotations were necessary.}
 @item{Finally, we conclude and list future work directions.
     @htodo{(turnstile, improvements to Racket itself, etc.)}}]})}}

