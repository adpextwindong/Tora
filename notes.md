# Notes

## Parser REBUILD

- [ ] Mutually recursive types and fundecs rebuild
- [ ] Declarations
  - [ ] Type Declaration
      - [ ] Type Id
      - [ ] { Record Type Fields }
      - [ ] Array of Type
  - [ ] Var Declaration
  - [ ] Function Declaration

- [ ] Expressions
  - [ ] Program AST decls and exprs
  - [ ] Parens
  - [ ] Nil
  - [ ] Let ...vardec.. in e end
  - [ ] Array expression
  - [ ] Record value creation
  - [ ] The No Value () expression
    - [ ] Unit No Value
    - [ ] Empty let expr body
  - [ ] Integer Literal
  - [ ] String Literal
  - [ ] Expression Sequences where last returns
  - [ ] Function Call
  - [ ] If then else
  - [ ] If then
  - [ ] While
  - [ ] For loop
  - [ ] break
  - [ ] Negation
  - [ ] Arithmetic
  - [ ] Comparrison
  - [ ] Boolean operators
  - [ ] LValues
    - [ ] identifier
    - [ ] dot operator (lvalue . id)
    - [ ] lvalue array access.
    - [ ] Assignment
  - [ ] Precedence of operators
  - [ ] Associativity

## Semantic Analysis

- [ ] `int` and `string` are predefined.
- [ ] Mutually recursive types. A collection of types may be recursive or mutually recursive. Mutually recursive types are declared by a consectuive sequence of type declarations without intervening value or function declarations. Each recursion cycle must pass through a record or array type.
- [ ] If the initializing expression is `nil`, then the long form must be used.
- [ ] Mutually recursive functions are declared by a sequence of consecutive function declarations.
- [ ] Expr Seq, evals all the expressions in order and the result of a seq is the result (if any) yielded by the last of the expressions.

### Reference

[https://www.lrde.epita.fr/~tiger/tiger.html#Syntactic-Specifications](https://www.lrde.epita.fr/~tiger/tiger.html#Syntactic-Specifications)
