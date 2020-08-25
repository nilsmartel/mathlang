use super::{
    atomic::{Identifier, Number},
    util::skip_ws,
    Parse,
};
use nom::{character::complete::char, IResult};

pub enum Expression {
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),

    Power(Box<Expression>, Box<Expression>),
    SquareRoot(Box<Expression>),
    Negate(Box<Expression>),
    Abs(Box<Expression>),
    Literal {
        identifier: Identifier,
        parameters: Vec<Expression>,
    },
    Number(Number),
}

impl Expression {
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

impl Parse for Expression {
    fn parse(input: &str) -> IResult<&str, Self> {
        add(input)
    }
}

fn infix_tree<'a, R, S>(
    parser: impl Fn(&'a str) -> IResult<&'a str, R>,
    sep: impl Fn(&'a str) -> IResult<&'a str, S>,
    builder: impl Fn(Box<R>, Box<R>) -> R,
) -> impl Fn(&'a str) -> IResult<&'a str, R> {
    // NOTE: separated_list1 isn't the right parser. Reason: "1 + 1 +" might be parsed, and the
    // free standing "+" will be consumed as well.
    use nom::{combinator::map, multi::separated_list1};
    move |input: &str| {
        map(separated_list1(skip_ws(&sep), &parser), |list| {
            let mut iter = list.into_iter();
            let tree = iter.next().unwrap();
            iter.fold(tree, |a, b| builder(Box::new(a), Box::new(b)))
        })(input)
    }
}

fn add(i: &str) -> IResult<&str, Expression> {
    infix_tree(subtract, char('+'), Expression::Add)(i)
}

fn subtract(i: &str) -> IResult<&str, Expression> {
    infix_tree(skip_ws(multiply), char('-'), Expression::Subtract)(i)
}

fn multiply(i: &str) -> IResult<&str, Expression> {
    infix_tree(skip_ws(divide), char('*'), Expression::Multiply)(i)
}

fn divide(i: &str) -> IResult<&str, Expression> {
    infix_tree(skip_ws(power), char('/'), Expression::Divide)(i)
}

fn power(i: &str) -> IResult<&str, Expression> {
    infix_tree(skip_ws(squareroot), char('^'), Expression::Power)(i)
}

fn squareroot(i: &str) -> IResult<&str, Expression> {
    if let Ok((rest, _sqrt)) = skip_ws(char('√'))(i) {
        use nom::combinator::map;
        return map(skip_ws(negate), |e| Expression::SquareRoot(e.boxed()))(rest);
    }
    negate(i)
}

fn negate(i: &str) -> IResult<&str, Expression> {
    if let Ok((rest, _sqrt)) = skip_ws(char('-'))(i) {
        use nom::combinator::map;
        return map(skip_ws(direct), |e| Expression::Negate(e.boxed()))(rest);
    }
    direct(i)
}

fn direct(i: &str) -> IResult<&str, Expression> {
    use nom::{branch::alt, combinator::map, sequence::delimited};

    alt((
        map(
            delimited(char('|'), Expression::parse_ws, skip_ws(char('|'))),
            |e| Expression::Abs(e.boxed()),
        ),
        delimited(char('('), Expression::parse_ws, skip_ws(char(')'))),
        literal,
        number,
    ))(i)
}

fn literal(i: &str) -> IResult<&str, Expression> {
    use nom::{combinator::map, multi::many0, sequence::pair};
    map(
        pair(Identifier::parse, many0(direct)),
        |(identifier, parameters)| Expression::Literal {
            identifier,
            parameters,
        },
    )(i)
}

fn number(i: &str) -> IResult<&str, Expression> {
    use nom::combinator::map;
    map(Number::parse, Expression::Number)(i)
}
