### Overview

There is an implementation of untyped lambda calculus in this repository. Variable names are one character long. Only lowercase letters are allowed.

### A Subset of Twelf's Grammar

    decl ::= id : term.

    term ::= type
           | id
           | {id : term} term
           | [id : term] term
           | term term

### Twelf's Syntax

The following was taken from the [wiki][1]. I believe `b` and `c` are supposed to be constants. Note that I copied it in verbatim, the lambda syntax would have to be changed to `[x : A] M` for the grammar above to support it.

    K ::= type | {x : A} K
    A ::= b | A M | {x : A} A'
    R ::= c | x | R M
    M ::= R | [x] M

If you haven't noticed, I find Twelf to be very elegant.

### Lambda Encodings

Ben Lynn has a very nice explanation of Scott encodings in his [site][2].

    -- Unit.
    case x of
      () -> f

    -- Scott encoded unit.
    \x -> x

    -- Peano numbers.
    data Nat = Zero | Succ Nat

    case x of
      Zero   -> f
      Succ n -> g n

    -- Scott encoded peano numbers.
    zero   = \s z -> z
    succ n = \s z -> s n

    one   = \s z -> s (\z s -> z)
    two   = \s z -> s (\z s -> s (\z s -> z))
    three = \s z -> s (\z s -> s (\z s -> s (\z s -> z)))

    -- Church encoded peano numbers.
    zero   = \s z -> z
    succ n = \s z -> s (n z s)

    one   = \s z -> s z
    two   = \s z -> s (s z)
    three = \s z -> s (s (s z))

### Induction Principles

The goal is to understand how to derive the induction principle for an arbitrary algebraic data type.

    Inductive unit : Set :=
      | tt : unit.

    (*
    unit_ind
         : forall P : unit -> Prop,
           P tt -> forall u : unit, P u
    *)
    Check unit_ind.

    Inductive bool : Set :=
      | true  : bool
      | false : bool.

    (*
    bool_ind
         : forall P : bool -> Prop,
           P true ->
           P false -> forall b : bool, P b
    *)
    Check bool_ind.

    Inductive nat : Set :=
      | O : nat
      | S : nat -> nat.

    (*
    nat_ind
         : forall P : nat -> Prop,
           P O ->
           (forall n : nat, P n -> P (S n)) ->
           forall n : nat, P n
    *)
    Check nat_ind.

    Inductive option (A : Type) : Type :=
      | some : A -> option A
      | none : option A.

    (*
    option_ind
         : forall (A : Type)
             (P : option A -> Prop),
           (forall a : A, P (some A a)) ->
           P (none A) ->
           forall o : option A, P o
    *)
    Check option_ind.

    Inductive sum (A B : Type) : Type :=
      | inl : A -> sum A B
      | inr : B -> sum A B.

    (*
    sum_ind
         : forall (A B : Type)
             (P : sum A B -> Prop),
           (forall a : A, P (inl A B a)) ->
           (forall b : B, P (inr A B b)) ->
           forall s : sum A B, P s
    *)
    Check sum_ind.

    Inductive prod (A B : Type) : Type :=
      | pair : A -> B -> prod A B.

    (*
    prod_ind
         : forall (A B : Type)
             (P : prod A B -> Prop),
           (forall (a : A) (b : B),
            P (pair A B a b)) ->
           forall p : prod A B, P p
    *)
    Check prod_ind.

    Inductive list (A : Type) : Type :=
      | nil  : list A
      | cons : A -> list A -> list A.

    (*
    list_ind
         : forall (A : Type)
             (P : list A -> Prop),
           P (nil A) ->
           (forall (a : A) (l : list A),
            P l -> P (cons A a l)) ->
           forall l : list A, P l
    *)
    Check list_ind.

[1]: http://twelf.org/wiki/Proving_metatheorems:Full_LF
[2]: https://crypto.stanford.edu/~blynn/compiler/scott.html
