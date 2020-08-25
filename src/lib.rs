mod parse;

pub use parse::atomic::{Identifier, Number};
pub use parse::expression::Expression;

pub fn parse_expression(input: &str) -> Option<Expression> {
    use parse::Parse;

    if let Ok((_rest, expr)) = Expression::parse_ws(input) {
        return Some(expr);
    }
    None
}
