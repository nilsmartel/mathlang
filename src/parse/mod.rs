mod atomic;
mod expression;
mod util;

pub(crate) trait Parse
where
    Self: Sized,
{
    fn parse(input: &str) -> nom::IResult<&str, Self>;

    fn parse_ws(input: &str) -> nom::IResult<&str, Self> {
        util::skip_ws(Self::parse)(input)
    }
}
