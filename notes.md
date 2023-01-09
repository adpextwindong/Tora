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
  - [x] LValues
    - [x] identifier
    - [x] dot operator (lvalue . id)
    - [x] lvalue array access.
    - [x] Assignment
  - [ ] Precedence of operators
  - [ ] Associativity
  - [ ] Page 97. The Tiger language treats _adjacent_ function declarations as (possibly) mutually recursive. The `FunctionDec` constructor of the abstract syntax takes a _list_ of function declarations, not just a single function. The intent is that this list is a maximal consecutive sequence of function declarations. Thus, functions declared by the same `FunctionDec` can be mutually recursive.
  - [ ] Page 99. The `TypeDec` constructor also takes a list of type declarations.
  - [ ] Page 99. There is no abstract syntax for `&` and `|` expressions; instead, `e1 & e2` is translated as `if e1 then e2 else 0` and `e1 | e2` is translated as thought it had been written `if e1 then 1 then e2`.
  - [ ] Page 99. Unary negation `(-i)` should be represented as subtraction (0 - i) in the abstract syntax.

## Semantic Analysis

- [ ] `int` and `string` are predefined.
- [ ] Mutually recursive types. A collection of types may be recursive or mutually recursive. Mutually recursive types are declared by a consectuive sequence of type declarations without intervening value or function declarations. Each recursion cycle must pass through a record or array type.
- [ ] If the initializing expression is `nil`, then the long form must be used.
- [ ] Mutually recursive functions are declared by a sequence of consecutive function declarations.
- [ ] Expr Seq, evals all the expressions in order and the result of a seq is the result (if any) yielded by the last of the expressions.

### Reference

[https://www.lrde.epita.fr/~tiger/tiger.html#Syntactic-Specifications](https://www.lrde.epita.fr/~tiger/tiger.html#Syntactic-Specifications)
