# Notes

## Parser

- [ ] Declarations
  - [ ] Type Declaration
      - [x] Type Id
      - [x] { Record Type Fields }
      - [ ] Array of Type
  - [x] Var Declaration
  - [x] Function Declaration

- [ ] Expressions
  - [ ] Paren
  - [x] Nil
  - [ ] Let ...vardec.. in e end
  - [ ] LValues
    - [ ] identifier
    - [ ] dot operator (lvalue . id)
    - [ ] lvalue array access
  - [ ] The No Value () expression (not unit??)
  - [x] Integer Literal
  - [ ] String Literal
  - [ ] Expression Sequences where last returns
  - [ ] Negation
  - [ ] Arithmetic
  - [ ] Comparrison
  - [ ] String comparison
  - [ ] Boolean operators
  - [ ] Function Call
  - [ ] Record value creation
  - [ ] Array expression
  - [ ] Array and record assignment expression
  - [ ] Assignment
  - [ ] If then else
  - [ ] If then
  - [ ] While
  - [ ] For loop

- [ ] Precedence of operators
- [ ] Associativity


## Semantic Analysis

- [ ] `int` and `string` are predefined.
- [ ] Mutually recursive types. A collection of types may be recursive or mutually recursive. Mutually recursive types are declared by a consectuive sequence of type declarations without intervening value or function declarations. Each recursion cycle must pass through a record or array type.
- [ ] If the initializing expression is `nil`, then the long form must be used.
- [ ] Mutually recursive functions are declared by a sequence of consecutive function declarations.


### Reference

[https://www.lrde.epita.fr/~tiger/tiger.html#Syntactic-Specifications](https://www.lrde.epita.fr/~tiger/tiger.html#Syntactic-Specifications)
