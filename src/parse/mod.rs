mod atomic;
mod util;

pub trait Parse
where
    Self: Sized,
{
    fn parse(input: &str) -> nom::IResult<&str, Self>;

    fn parse_ws(input: &str) -> nom::IResult<&str, Self> {
        use nom::bytes::complete::take_while;
        use nom::sequence::preceded;

        preceded(
            take_while(|c| c == ' ' || c == '\n' || c == '\r' || c == '\t'),
            Self::parse,
        )(input)
    }
}
