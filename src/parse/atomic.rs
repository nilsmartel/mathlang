use super::{util, Parse};

#[derive(Clone, Debug, PartialOrd, PartialEq, Eq, Ord)]
pub struct Identifier(String);

impl Parse for Identifier {
    fn parse(input: &str) -> nom::IResult<&str, Self> {
        use nom::{
            bytes::complete::{take_while, take_while1},
            combinator::{map, recognize},
            sequence::pair,
        };
        use util::{is_alpha, is_ident_char};

        map(
            recognize(pair(take_while1(is_alpha), take_while(is_ident_char))),
            |s: &str| Identifier(s.to_string()),
        )(input)
    }
}

impl From<&str> for Identifier {
    fn from(input: &str) -> Self {
        Identifier::parse(input).expect("Failed to parse literal").1
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn literal() {
        assert_eq!(
            Identifier::parse("my_number_123_ab_13"),
            Ok(("", Identifier("my_number_123_ab_13".to_string())))
        );
    }

    #[test]
    fn number() {
        fn eq(input: &str, n: f64) {
            assert_eq!(Number::parse(input), Ok(("", Number(n))));
        }

        eq("3.14", 3.14);
        eq("314e-2", 3.14);
        eq("3140e-3", 3.14);
        eq("0.3140e+1", 3.14);
        eq("0.3140e1", 3.14);
        eq("1", 1.0);
        eq("1.0", 1.0);
        eq("00001.0001", 1.0001);
    }
}

#[derive(Clone, Copy, Debug, PartialOrd, PartialEq)]
pub struct Number(f64);
impl Eq for Number {}

impl Parse for Number {
    fn parse(input: &str) -> nom::IResult<&str, Number> {
        use nom::{
            character::complete::{char, one_of},
            combinator::{map, opt, recognize},
            sequence::{preceded, tuple},
        };
        use util::decimal;

        let f =
          // Case two: 42e42 and 42.42e42
          recognize(
            tuple((
              decimal,
              opt(preceded(
                char('.'),
                decimal,
              )),
              opt(tuple((
              one_of("eE"),
              opt(one_of("+-")),
              decimal))
              )
            ))
          );

        map(f, |s: &str| Number(s.parse().unwrap()))(input)
    }
}

impl From<f64> for Number {
    fn from(i: f64) -> Self {
        Number(i)
    }
}

impl From<&str> for Number {
    fn from(i: &str) -> Self {
        let f = i
            .parse::<f64>()
            .expect(&format!("failed to parse {} into Number", i));
        Number(f)
    }
}
