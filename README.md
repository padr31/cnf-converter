# cnf-converter
Convert propositional logic formulas to CNF

## How to use
1. clone repository
2. install haskell
3. `ghci converter.hs`
4. use command `convert` to do the conversion, use prefix notation and leave a space between every token

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
