# cnf-converter and DPLL solver
Convert propositional logic formulas to CNF, convert CNF to clauses and find out if they are refutable or satisfiable using DPLL method

# How to set up
1. clone repository
2. install haskell
3. `ghci converter.hs`

# Converting to CNF
Use command convert followed by your formula in prefix notation

## Recognised tokens
- `and` for conjunction
- `or` for disjunction
- `not` for negation
- `implies` for implication       
- `(` for bracketing
- `)` for bracketing
- `t` for true
- `f` for false
- otherwise - tokens are recognised as identifiers

## Examples

`convert "or ( and A B ) C"`
> "((A or C) and (B or C))"

`convert "or ( and A B ) ( or C D )"`
> "(((A or C) and (A or D)) and ((B or C) and (B or D)))"

# Converting to clauses
Use command propToClauses followed by the propositional formula in prefix notation as above to convert it to clauses.

## Examples

`propToClauses "or ( and A B ) C"`
> "{A, C}{B, C}"

`propToClauses "or ( and A B ) ( or C D )"`
> "{A, C, D}{B, C, D}"

# DPLL
The DPLL algorithm removes tautologies from a clause set, deals with unit literals, deals with pure literals, and performs a case split in case it needs. The result is telling whether the clause set is refuted or satisfiable.

## Literals in clauses
- `Pos String` - positive literal, we use upper case letters by convention, for example Pos "A"
- `Neg String` - negative literal, we use upper case letters by convention, for example Neg "A"
- `Bol String` - boolean, we have 2 options - Bol "t", Bol "f"

## Examples
A clause is a list of literals `[Pos "P", Pos "Q"]`.
A clause set if a list of clauses `[[Pos "P", Pos "Q"], [Pos "Q", Neg "P"], [Neg "Q", Pos "P"], [Neg "P", Neg "Q"]]`.

We can run DPLL on sets of clauses and get a result:
`dpll [[Pos "P", Pos "Q"], [Pos "Q", Neg "P"], [Neg "Q", Pos "P"], [Neg "P", Neg "Q"]]`
> "Refuted" - because we got the empty clause [[],...] at the end

`dpll [[Neg "P", Pos "R"], [Pos "P"], [Neg "Q", Pos "R"], [Neg "R"]]`
> "Refuted" - because we got the empty clause [[],...] at the end

`dpll [[Pos "P", Pos "Q"], [Neg "Q"], [Neg "R"]]`
> "Satisfiable" - because we got the empty clause set [] at the end

