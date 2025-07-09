# Hilbert-style proof-checker system

The following project attempts to implement a Hilbert-style proof-checker system that is capable of expressing Hilbert-style proofs and checking said proofs for correctness.

## Running the project

The project uses Cabal as its build system.

To build the project, navigate to the project directory, namely `/hilbert-systems`:

```
cabal build {hilbert-systems}
```

Given the program loads and reads proof primarily from files, it expects a single filename to read from as a command line argument:

```
cabal run hilbert-systems -- -- {file_path}
```

*Note that filenames must end in the .hmic extension.*

To run one of the example files (provided in the examples directory):

```
cabal run hilbert-systems -- -- examples/example1.hmic
```

## Usage

To see a basic outline for the syntax of a Hilbert-style proof, go to [the grammar section below](readme#Grammar) or navigate to the [examples directory](./examples/).

Each program consists of a set of proofs, as defined in the file the program tries to read from.

Upon running the program, the expected behaviour is as follows:

- display a message indicating that the program has successfully parsed all proofs and has verified their correctness;

- display the identifier of the first theorem that has failed, as read from top to bottom, and the reason for failure (the corresponding incorrect "proof statement");

- display a parse error. Parse error messages, at this point in time, are not particularly verbose - they mostly warn of missing keywords or symbols somewhat pivotal to the parser's success. That could *prove* to be a potential future improvement.


## Grammar

- Introducing a new theorem:

```
theorem <identifier> :=
  assume <assumption_1>, <assumption_2>, ..., <assumption_n>.
  goal <formula>.
  begin.
    <statement_1> by <rule> {<rule_arguments>}.
    ...
    <statement_N> by <rule> {<rule_arguments>}.
  qed.
```

*Note*: The grammar is not indentation-sensitive.

- Formulae:

```
<formula>         ::= void | <variable> | !<formula> | <formula> <logical_symbol> <formula> | <quantifier> <variable>, <formula> | <formula>[<variable> ~> <formula>]
<variable>        ::= {'A', 'B', ..., 'Z'}+
<logical_symbol>  ::= "&" | "|" | "->"
<quantifier>      ::= "\forall" | "\exists"
```

**Goals, assumptions and statements are just formulae**.

- Rules:

```
<rule> = AS
       | AX <axiom_name>
       | MP
       | GEN 
```

- Axioms:

names are kinda shit ngl

```
1.  S                     :  (A -> B -> C) -> (A -> B) -> A -> C
2.  K                     :  A -> B -> A
3.  CONJ_ELIM             :  A & B -> A; A & B -> B
4.  CONJ_INTRO            :  A -> B -> A & B
5.  DISJ_INTRO            :  A -> A | B; B -> A | B
6.  DISJ_ELIM             :  (A -> C) -> (B -> C) -> A | B -> C
7.  UNIV_WITNESS          :  \forall x, A(x) -> A(t), t - formula
8.  UNIV_IMPLIES          :  \forall x, (B -> A(x)) -> (B -> \forall x, A(x)) where x not in FV(B)
9.  SUB_EXIST             :  A(t) -> \exists x, A(x), t - formula
10. EX_IMPLIES            :  \forall x, (A(x) -> B) -> (\exists x, A(x) -> B) where x not in FV(B)
11. EX_FALSO              :  void -> A
12. STAB                  :  !!A -> A
```


## Implementation-specific details

### Parsing

The projects uses a custom implementation of a mixture of *mostly* generic parsing combinators and ad hoc Pratt parser functions that are used to preserve the precedence and associativity of the operators used.

### Data representation

A *theorem* consists of a few things: an identifier, a goal, a context of all the assumptions made at a given point in time, and a list of sequentially ordered "proof statements". As already discussed above, goals and assumptions are *formulae*.

```
data Theorem = Theorem Identifier Goal Context ProofStatements
```

A proof statement has two components - a formula and a rule describing how that formula has been derived:

```hs
data Rule
  = AS        -- assumption
  | AX Axiom  -- axiom
  | MP        -- modus ponens
  | GEN       -- generalisation
  deriving (Show)
```

Since the program uses pattern matching to determine whether a *concrete* formula matches an *axiom schema* (if the `AX` rule is used), there needs to be a distinction between *meta-* and *concrete* formulae. Internally, they share the same algebraic data type and only differ on a type level. This is achieved by making the data type a *phantom* data type, where the type parameter does not affect the runtime representation of the ADT:

```hs
data Formula a
  = Void
  | Variable String
  | Negation (Formula a)
  | (Formula a) :&: (Formula a)
  | (Formula a) :|: (Formula a)
  | (Formula a) :->: (Formula a)
  | Forall String (Formula a)
  | Exists String (Formula a)
  | (Formula a) `With` (Substitution a)
  deriving (Show, Eq, Ord)

data Concrete
data Meta

type ConcreteFormula = Formula Concrete
type Metaformula = Formula Meta
```

Axioms are represented by a rather large enum-like ADT, consisting of 12 different axioms:

```hs
data Axiom
  = S
  | K
  | CONJ_ELIM Position   -- left or right
  | CONJ_INTRO
  | DISJ_INTRO Position  -- left or right
  | DISJ_ELIM
  | UNIV_WITNESS
  | UNIV_IMPLIES
  | SUB_EXIST
  | EX_IMPLIES
  | EX_FALSO
  | STAB
  deriving (Show, Eq, Ord)
```

Additionally, each axiom schema has a corresponding formula definition (see [here](./src/Program/Axioms.hs)).

### Some important details

TODO

- syntactic equality vs pattern matching of formulae
- proofcheck'
- prove


## Potential future improvements

- allow for proof by "sorry"
- pretty-printing of proofs (expanded axiom definitions + pattern matching substitutions made; witnesses of MP and GEN)
- allow for a program state where previously defined and proved theorems can be used in other theorems
