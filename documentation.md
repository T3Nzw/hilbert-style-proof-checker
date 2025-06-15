# Hilbert-style proof-checker system

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
<formula>      ::= void | <variable> | !<formula> | <formula> <logical_symbol> <formula> | <quantifier> <variable>. <formula> | <formula>[<variable> ~> <formula>]
<variable>        ::= {'a', 'b', ..., 'z'}+
<logical_symbol>  ::= "&" | "|" | "->"
<quantifier>      ::= "\forall" | "\exists"
```

**Assumptions and statements are just formulae**.

- Rules:

```
<rule> = AS
       | AX <axiom_name>
       | MP
       | GEN <formula>
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
7.  UNIV_WITNESS <subst>  :  \forall x. A -> A [x ~> <subst>]
8.  UNIV_IMPLIES          :  \forall x. (B -> A) -> (B -> \forall x. A) where x not in FV(B)
9.  SUB_EXIST <subst>     :  A[x ~> <subst>] -> \exists x. A
10. EX_IMPLIES            :  \forall x. (A -> B) -> (\exists x. A -> B) where x not in FV(B)
11. EX_FALSO              :  void -> A
12. STAB                  :  !!A -> A
```
