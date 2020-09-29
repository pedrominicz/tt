### Overview

There is an implementation of untyped lambda calculus in this repository. Variable names are one character long. Only lowercase letters are allowed.

Adding inductive types to a type theory is a difficult task. It can involve adding multiple systems to the language: positivity checker, termination checker, algebraic data types, pattern matching, etc.

I've been exploring a hopefully simpler approach: induction for lambda-encoded data types. I'll leave a little roadmap of material to study here.

- Lambda encoding
  - [Beyond Church encoding][1]
- Curry-style type theory
  - Barendregt's classic Lambda Calculi with Types
  - The Implicit Calculus of Constructions
  - Classical Program Extraction in the Calculus of Constructions
  - The Implicit Calculus of Constructions as a Programming Language with Dependent Types
- Induction and fixpoints
  - [Recursive Types for Free!][2]
  - Inductively Defined Types in the Calculus of Constructions
- Cedille related
  - Generic Derivation of Induction for Impredicative Encodings in Cedille
- Self types
  - Self Types for Dependently Typed Lambda Encodings

### Impredicativity and Proof Irrelevance in the Calculus of Constructions

I am not fond of infinite universe hierarchies. I rather have an impredicative universe. However, [`impredicative polymorphism /\ excluded middle /\ large elimination -> false`][3].

- [Axiom K][4] (a.k.a. uniqueness of identity proofs)
- Calculus of Inductive Constructions
  - [Coq][5]
  - [Lean][6]
- The Implicit Calculus of Constructions as a Programming Language with Dependent Types

Note that `impredicative polymorphism /\ excluded middle -> proof irrelevance`.

- Irrelevance on the [Agda wiki][7] (the irrelevance axiom is of special interest)
- On the strength of proof-irrelevant type theories
- Propositions as [Types]
- Erasure and Polymorphism in Pure Type Systems

### A Subset of Twelf's Grammar

    decl ::= id : term.

    term ::= type
           | id
           | {id : term} term
           | [id : term] term
           | term term

### Twelf's Syntax

The following was taken from the [wiki][8]. I believe `b` and `c` are constants. Note that I copied it in verbatim, the lambda syntax would have to be changed to `[x : A] M` for the grammar above to support it.

    K ::= type | {x : A} K
    A ::= b | A M | {x : A} A'
    R ::= c | x | R M
    M ::= R | [x] M

If you haven't noticed, I find Twelf to be very elegant.

[1]: http://okmij.org/ftp/tagless-final/course/Boehm-Berarducci.html
[2]: https://homepages.inf.ed.ac.uk/wadler/papers/free-rectypes/free-rectypes.txt
[3]: https://github.com/FStarLang/FStar/issues/360
[4]: https://ncatlab.org/nlab/show/axiom+K+%28type+theory%29
[5]: https://coq.inria.fr/distrib/current/refman/language/cic.html
[6]: https://lean-forward.github.io/logical-verification/2018/41_notes.html
[7]: https://agda.readthedocs.io/en/latest/language/irrelevance.html
[8]: http://twelf.org/wiki/Proving_metatheorems:Full_LF
