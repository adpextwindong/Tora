# Notes

## Parser

- [x] Declarations
  - [x] Type Declaration
      - [x] Type Id
      - [x] { Record Type Fields }
      - [x] Array of Type
  - [x] Var Declaration
  - [x] Function Declaration

- [ ] Expressions
  - [x] Program AST decls and exprs
  - [x] Parens
  - [x] Nil
  - [x] Let ...vardec.. in e end
  - [x] Array expression
  - [x] Record value creation
  - [x] The No Value () expression
    - [x] Unit No Value
    - [x] Empty let expr body
  - [x] Integer Literal
  - [x] String Literal
  - [x] Expression Sequences where last returns
  - [x] Function Call
  - [x] If then else
  - [x] If then
  - [x] While
  - [x] For loop
  - [x] break
  - [x] Negation
  - [x] Arithmetic
  - [x] Comparrison
  - [x] Boolean operators
  - [ ] Precedence of operators
  - [ ] Associativity
  - [ ] LValues
    - [ ] identifier
    - [ ] dot operator (lvalue . id)
    - [ ] lvalue array access
    - [ ] Array and record assignment expression
    - [ ] Assignment

## Semantic Analysis

- [ ] `int` and `string` are predefined.
- [ ] Mutually recursive types. A collection of types may be recursive or mutually recursive. Mutually recursive types are declared by a consectuive sequence of type declarations without intervening value or function declarations. Each recursion cycle must pass through a record or array type.
- [ ] If the initializing expression is `nil`, then the long form must be used.
- [ ] Mutually recursive functions are declared by a sequence of consecutive function declarations.
- [ ] Expr Seq, evals all the expressions in order and the result of a seq is the result (if any) yielded by the last of the expressions.

### Reference

[https://www.lrde.epita.fr/~tiger/tiger.html#Syntactic-Specifications](https://www.lrde.epita.fr/~tiger/tiger.html#Syntactic-Specifications)
