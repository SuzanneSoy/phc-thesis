#lang scribble/manual

@require["util.rkt"]
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
 edit-build-test cycle should be as frequent as possible@todo{@~cite{
   smalltalk-programmer-efficiency-cycle}}.

 Given their broad role, the complexity of the transformations involved, and
 the stringent requirements, writing compilers is a difficult task. Multiple
 pitfalls await the compiler engineer, which we will discuss in more detail
 below. This thesis aims to improve the compiler-writing toolkit currently
 available, in order to help compiler developers produce compilers which are
 closer to correctness, and easier to maintain.

 @require[scribble/core scribble/html-properties scribble/latex-properties]
 @elem[#:style (style "hrStyle"
                      (list (alt-tag "hr")
                            (css-addition
                             #".hrStyle { margin-bottom: 1em; }")
                            (tex-addition
                             (string->bytes/utf-8 #<<EOTEX
\def\hrStyle#1{\noindent{\centerline{\rule[0.5ex]{0.5\linewidth}{0.5pt}}}}
EOTEX
                                                  ))))]{}

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
    representation}, instead of properly distinguishing the features of the input
   and output of each pass;}
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

  Nontrivial programs are inherently graphs: they may contain mutually
  recursive functions (which directly refer to each other, and therefore will
  form a cycle in a representation of the program), circular and (possibly
  mutually) recursive datatypes may syntactically contain (possibly indirect)
  references to themselves, and the control flow graph of a function or method
  may, as its name implies, contain instructions which perform conditional or
  unconditional backwards branches.

  However, nearly every compiler textbook will mention the use of
  @tech[#:key "AST"]{ Abstract Syntax Trees} (ASTs) to represent the program.
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
    bugs: name clashes can occur if the qualification chosen is not sufficient
    to always distinguish nodes; @htodo{furthermore} compiler passes which
    duplicate nodes (for example specialising functions) or merge them must be
    careful to correctly update identifiers.}
 @item{Alternatively, backward references may be encoded as a form of path
    from the referring node. @DeBruijn indices can be used in such an encoding,
    for example.

    Once again, manipulating these references is risky, and @DeBruijn indices
    are particularly brittle, for example when adding a wrapper around a node
    (i.e. adding an intermediate node on the path from the root), the @DeBruijn
    indices used in some of descendents of that node (but not all) must be
    updated. It is understandably easy to incorrectly implement updates to these
    indices, and a single off-by-one error can throw the graph's representation
    into an inconsistent state.}
 @item{The program's representation could also contain actual pointers
    (thereby really representing the program as an ``Abstract Syntax Graph''),
    using mutation to patch nodes after they are initially created.

    @todo{Mutation: verification (two phases for invariants), generally frowned
     upon, reference some of Roland's and others' work on freezing objects. (as
     long as it is ensured that no improper manipulation of the objects is done
     before freezing).}}
 @item{The compiler could also manipulate lazy data structures, where the
    actual value of a node in the graph is computed on the fly when that node is
    accessed.

    @todo{Lazy: harder to debug}}
 @item{Finally, Higher-Order Abstract Syntax, or HOAS for short, is a
    technique which encodes variable bindings as anonymous functions in the host
    language (whose parameters reify bindings at the level of the host
    language). Variable references are then nothing more than actual uses of the
    variable at the host language's level. Substitution, a common operation in
    compilers and interpreters, then becomes a simple matter of calling the
    anonymous function with the desired substitute. HOAS has the additional
    advantage that it enforces well-scopedness, as it is impossible to refer to
    a variable outside of its scope in the host language.

    Parametric HOAS, dubbed PHOAS, also allows encoding the type of the
    variables in the representation. @todo{Can extra information other than the
     type be stored?}

    There are a few drawbacks with HOAS and PHOAS:

    The ``target'' of a backward reference must be above all uses in the tree.
    This might not always be true. For example, pre/post-conditions could, in an
    early pass in the compiler, be located outside of the normal scope of a
    function's signature, but still refer to the function's parameters. If the
    pre/post-condition language allows breaking encapsulation, these could even
    refer to some temporary variables declared inside the function.

    @;{
     @; True for HOAS, not sure for PHOAS.
     @todo{The ``target'' of a backward reference does not initially contain
      additional data (e.g. the variable name to be used for error messages, its
      static or concrete type and so on) although extending the encoding to
      support this should be feasible.}
    }

    @todo{PHOAS naturally lends itself to the implementation of substitutions,
     and therefore is well-suited to the writing of interpreters. However, the
     representation cannot be readily traversed and accesses like would be done
     with normal structures, and therefore the model could be counter-intuitive
     for some programmers.}

    @todo{It seems difficult to encode an arbitrary number of variables bound in
     a single construct (e.g. to represent bound type names across the whole
     program, or an arbitrary number of mutually-recursive functions declared
     via @racketid[let … and … in …], with any number of @racketid[and] clauses
     in the compiled language.}}
 ]
  
  Although some of these seem like viable solutions (e.g. explicitly freezing
  objects), they still involve low-level mechanisms to create the graph;
  furthermore code traversing the graph needs to be deal with cycles, in order
  to avoid running into an infinite loop (or infinite recursion).

  anecdotally

  
  updates: all logical pointers to an updated node must be updated too.

  @htodo{Think about ensuring that nodes from two distinct graphs are not mixed
   in unexpected ways (placing a dummy phantom type somewhere should be enough
   to prevent it).}
 
 }

 @asection{
  @atitle{Expressing the data dependencies of a path via row types}
 }
 

 @;{
  The static analysis, optimisation and code generation phases will often work
  on that @usetech{intermediate representation}.

  These transformations are often non-trivial and may require aggregating and
  analysing data scattered across the program.

  We build upon the achievements of the Nanopass Compiler Framework project,
  which is presented in more detail in section XYZ. Simply put, Nanopass helps
  the programmer define a myriad of compiler passes, each doing a very small
  amount of code (and therefore easy to test and maintain), and each with a
  different input and output type.
  
 }
}