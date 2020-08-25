use nom::IResult;

#[inline]
pub fn is_alpha(c: char) -> bool {
    c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
}

#[inline]
pub fn is_number(c: char) -> bool {
    c >= '0' && c <= '9'
}

#[inline]
pub fn is_ident_char(c: char) -> bool {
    is_alpha(c) || c == '_' || is_number(c)
}

pub fn decimal(i: &str) -> nom::IResult<&str, &str> {
    use nom::bytes::complete::take_while1;

    take_while1(is_number)(i)
}

/// Remove all whitespace, newlines, tabs etc.
/// Will always suceed
pub(crate) fn whitespace(s: &str) -> IResult<&str, &str> {
    nom::bytes::complete::take_while(|c| c == ' ' || c == '\n' || c == '\r' || c == '\t')(s)
}

/// Wrap around a Parser to automatically ignore preceding whitespace
pub(crate) fn skip_ws<'a, T>(
    f: impl Fn(&'a str) -> IResult<&'a str, T>,
) -> impl Fn(&'a str) -> IResult<&'a str, T> {
    move |i: &str| {
        let (i, _) = whitespace(i).unwrap();
        f(i)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_whitespace() {
        assert_eq!(Ok(("hello", "")), whitespace("hello"));

        assert_eq!(Ok(("hello", "\n ")), whitespace("\n hello"));

        assert_eq!(Ok(("", "    ")), whitespace("    "));
        assert_eq!(Ok(("", "")), whitespace(""));
    }

    #[test]
    fn test_skip_ws() {
        use nom::bytes::complete::tag;
        assert_eq!(Ok(("", "hello")), skip_ws(tag("hello"))("hello"));
        assert_eq!(Ok(("", "hello")), skip_ws(tag("hello"))("   hello"));
    }
}
