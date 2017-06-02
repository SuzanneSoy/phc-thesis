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
 the stringent requirements, writing compilers is a difficult task.

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

 Some pitfalls await the compiler-writer: it is easy to reuse excessively a
 single intermediate representation; and there is a high risk associated with
 the writing of large, monolithic passes, which are hard to test, debug, and
 extend. We will discuss these pitfalls in more detail in the following
 paragraphs. Both issues are prone to manifestations of some form or another of
 the ``god object'' anti-pattern@note{The ``god object'' anti-pattern describes
  object-oriented classes which @emph{do} too much or @emph{know} too much. The
  size of these classes tends to grow out of control, and there is usually a
  tight coupling between the methods of the object, which in turn means that
  performing small changes may require understanding the interactions between
  random parts of a very large file, in order to avoid breaking existing
  functionality.}.


 The static analysis, optimisation and code generation phases could in
 principle work on the same intermediate representation. Several issues arise
 from this situation, however. First, new information gained by the static
 analysis may be added to the existing representation via mutation, or the
 optimiser could directly alter the @tech{IR}. This means that the @tech{IR}
 will initially contain holes (e.g. represented by @racketid[null] values),
 which will get filled in gradually. Manipulating these parts is then extremely
 risky, as it is easy to accidentally attempt to retrieve a value before it was
 actually computed. Using the same @tech{IR} throughout the compiler also makes
 it difficult for later passes to assume that some constructions have been
 eliminated by previous simplification passes. One has to rely on the order of
 execution of the passes in order to know what the data structure contains,
 instead of having this information indicated by the @tech{IR}'s type.

 @;{
  The static analysis, optimisation and code generation phases will often work
  on that intermediate representation.

  These transformations are often non-trivial and may require aggregating and
  analysing data scattered across the program.

  triggering anti-patterns like ``god object''
 }
}