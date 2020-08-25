# mathlang

Parser for mathematical expressions

example usage:
```rust
use mathlang::parse_expression;

let expr = parse_expression("3a^2 + 2b^2 + √2").unwrap()
```

## Planned

- implement symbolic derivation / integration
- compiling of expressions
- ability to describe mathematical functions
