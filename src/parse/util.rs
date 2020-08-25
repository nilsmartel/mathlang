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
