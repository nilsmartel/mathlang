mod atomic;
mod expression;
mod util;

pub use atomic::*;
pub use expression::*;

pub(crate) trait Parse
where
    Self: Sized,
{
    fn parse(input: &str) -> nom::IResult<&str, Self>;

    fn parse_ws(input: &str) -> nom::IResult<&str, Self> {
        util::skip_ws(Self::parse)(input)
    }
}

pub fn parse_expression_ast(input: &str) -> Option<Expression> {
    match Expression::parse_ws(input) {
        Ok((rest, ast)) if is_all_whitespace(rest) => Some(ast),
        _ => None,
    }
}

fn is_all_whitespace(i: &str) -> bool {
    let (rest, _) = util::whitespace(i).unwrap();

    rest == ""
}
