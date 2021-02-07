#[forbid(unsafe_code)]
use std::collections::BTreeMap;

// alphabetical order
const ATOM_TAG: u8 = 100;
const ATOM_UTF8_TAG: u8 = 118;
const BINARY_TAG: u8 = 109;
const INTEGER_TAG: u8 = 98;
const LARGE_BIG_TAG: u8 = 111;
const LARGE_TUPLE_TAG: u8 = 105;
const LIST_TAG: u8 = 108;
const MAP_TAG: u8 = 116;
const NEW_FLOAT_TAG: u8 = 70;
const NIL_TAG: u8 = 106;
const SMALL_ATOM: u8 = 115;
const SMALL_ATOM_UTF8_TAG: u8 = 119;
const SMALL_BIG_TAG: u8 = 110;
const SMALL_INTEGER_TAG: u8 = 97;
const SMALL_TUPLE_TAG: u8 = 104;
const VERSION_NUMBER_TAG: u8 = 131;

#[derive(Clone, Debug)]
pub enum Error<'e> {
    Incomplete(Needed),
    Error(&'e [u8]),
}

#[derive(Clone, Debug)]
pub enum Needed {
    Needed(usize),
    Unknown,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub enum Term<'a> {
    Atom(&'a str),
    Binary(&'a str),
    Integer(i32),
    List(Vec<Term<'a>>),
    Map(BTreeMap<Term<'a>, Term<'a>>),
    Float(ordered_float::OrderedFloat<f64>),
    Nil,
    SmallInteger(u8),
    Tuple(Vec<Term<'a>>),
    BigInt(num_bigint::BigInt),
}

pub fn parse(s: &[u8]) -> Result<Term, Error> {
    let (_, term) = match s {
        [VERSION_NUMBER_TAG, rest @ ..] => term(rest)?,
        [] => return Err(Error::Incomplete(Needed::Unknown)),
        input => return Err(Error::Error(input)),
    };

    Ok(term)
}

fn term(s: &[u8]) -> Result<(&[u8], Term), Error> {
    match s {
        [ATOM_TAG, rest @ ..] => atom(rest),
        [BINARY_TAG, rest @ ..] => binary(rest),
        [SMALL_INTEGER_TAG, rest @ ..] => small_integer(rest),
        [INTEGER_TAG, rest @ ..] => integer(rest),
        [NEW_FLOAT_TAG, rest @ ..] => new_float(rest),
        [MAP_TAG, rest @ ..] => map(rest),
        [LIST_TAG, rest @ ..] => list(rest),
        [NIL_TAG, rest @ ..] => Ok((rest, Term::Nil)),
        [SMALL_TUPLE_TAG, rest @ ..] => small_tuple(rest),
        [LARGE_TUPLE_TAG, rest @ ..] => large_tuple(rest),
        [ATOM_UTF8_TAG, rest @ ..] => atom_utf8(rest),
        [SMALL_ATOM_UTF8_TAG, rest @ ..] => small_atom_utf8(rest),
        [SMALL_ATOM, rest @ ..] => small_atom(rest),
        [SMALL_BIG_TAG, rest @ ..] => small_big(rest),
        [LARGE_BIG_TAG, rest @ ..] => large_big(rest),
        [] => Err(Error::Incomplete(Needed::Unknown)),
        input => Err(Error::Error(input)),
    }
}

fn atom(s: &[u8]) -> Result<(&[u8], Term), Error> {
    match s {
        [a, b, s @ ..] => {
            let len = u16::from_be_bytes([*a, *b]) as usize;
            if len <= s.len() {
                let (string_bytes, s) = s.split_at(len);
                let inner = std::str::from_utf8(string_bytes);

                if let Ok(inner) = inner {
                    Ok((s, Term::Atom(inner)))
                } else {
                    Err(Error::Error(s))
                }
            } else {
                Err(Error::Error(s))
            }
        }
        input => Err(Error::Error(input)),
    }
}

fn atom_utf8(s: &[u8]) -> Result<(&[u8], Term), Error> {
    match s {
        [a, b, s @ ..] => {
            let len = u16::from_be_bytes([*a, *b]) as usize;
            if len <= s.len() {
                let (string_bytes, s) = s.split_at(len);
                let inner = std::str::from_utf8(string_bytes);

                if let Ok(inner) = inner {
                    Ok((s, Term::Atom(inner)))
                } else {
                    Err(Error::Error(s))
                }
            } else {
                Err(Error::Error(s))
            }
        }
        input => Err(Error::Error(input)),
    }
}

fn binary(s: &[u8]) -> Result<(&[u8], Term), Error> {
    match s {
        [b1, b2, b3, b4, s @ ..] => {
            let len = u32::from_be_bytes([*b1, *b2, *b3, *b4]) as usize;
            if len <= s.len() {
                let (string_bytes, s) = s.split_at(len);
                let inner = std::str::from_utf8(string_bytes);

                if let Ok(inner) = inner {
                    Ok((s, Term::Binary(inner)))
                } else {
                    Err(Error::Error(s))
                }
            } else {
                Err(Error::Error(s))
            }
        }
        input => Err(Error::Error(input)),
    }
}

fn integer(s: &[u8]) -> Result<(&[u8], Term), Error> {
    match s {
        [b1, b2, b3, b4, s @ ..] => {
            let i = i32::from_be_bytes([*b1, *b2, *b3, *b4]);
            Ok((s, Term::Integer(i)))
        }
        input => Err(Error::Error(input)),
    }
}

fn small_integer(s: &[u8]) -> Result<(&[u8], Term), Error> {
    match s {
        [i, rest @ ..] => Ok((rest, Term::SmallInteger(*i))),
        input => Err(Error::Error(input)),
    }
}

fn large_big(s: &[u8]) -> Result<(&[u8], Term), Error> {
    match s {
        [b1, b2, b3, b4, sign_byte, s @ ..] => {
            let n = u32::from_be_bytes([*b1, *b2, *b3, *b4]) as usize;
            if n <= s.len() {
                let (digits, s) = s.split_at(n);

                let sign = if sign_byte == &1u8 {
                    num_bigint::Sign::Minus
                } else {
                    num_bigint::Sign::Plus
                };

                if !digits.is_empty() {
                    let inner = num_bigint::BigInt::from_radix_le(sign, &digits, 256);

                    if let Some(inner) = inner {
                        Ok((s, Term::BigInt(inner)))
                    } else {
                        Err(Error::Error(s))
                    }
                } else {
                    Err(Error::Error(s))
                }
            } else {
                Err(Error::Error(s))
            }
        }
        input => Err(Error::Error(input)),
    }
}

fn small_big(s: &[u8]) -> Result<(&[u8], Term), Error> {
    match s {
        [n, sign_byte, s @ ..] => {
            let n = *n as usize;
            if n <= s.len() {
                let (digits, s) = s.split_at(n as usize);

                let sign = if sign_byte == &1u8 {
                    num_bigint::Sign::Minus
                } else {
                    num_bigint::Sign::Plus
                };

                if !digits.is_empty() {
                    let inner = num_bigint::BigInt::from_radix_le(sign, &digits, 256);

                    if let Some(inner) = inner {
                        Ok((s, Term::BigInt(inner)))
                    } else {
                        Err(Error::Error(s))
                    }
                } else {
                    Err(Error::Error(s))
                }
            } else {
                Err(Error::Error(s))
            }
        }
        input => Err(Error::Error(input)),
    }
}

fn new_float(s: &[u8]) -> Result<(&[u8], Term), Error> {
    match s {
        [b1, b2, b3, b4, b5, b6, b7, b8, s @ ..] => {
            let f = f64::from_be_bytes([*b1, *b2, *b3, *b4, *b5, *b6, *b7, *b8]);
            Ok((s, Term::Float(f.into())))
        }
        input => Err(Error::Error(input)),
    }
}

fn list(s: &[u8]) -> Result<(&[u8], Term), Error> {
    match s {
        [b1, b2, b3, b4, s @ ..] => {
            let len = u32::from_be_bytes([*b1, *b2, *b3, *b4]) as usize;

            let mut elements = Vec::with_capacity(len);

            let mut s = s;

            for _ in 0..len {
                let (rest, element) = term(s)?;
                s = rest;
                elements.push(element);
            }

            let (s, tail) = term(s)?;

            if tail != Term::Nil {
                elements.push(tail);
            }

            Ok((s, Term::List(elements)))
        }
        input => Err(Error::Error(input)),
    }
}

fn map(s: &[u8]) -> Result<(&[u8], Term), Error> {
    match s {
        [b1, b2, b3, b4, s @ ..] => {
            let number_of_pairs = u32::from_be_bytes([*b1, *b2, *b3, *b4]) as usize;
            let mut elements = BTreeMap::new();
            let mut s = s;
            for _ in 0..number_of_pairs {
                let (rest, k) = term(s)?;
                s = rest;
                let (rest, v) = term(s)?;
                s = rest;
                elements.insert(k, v);
            }

            Ok((s, Term::Map(elements)))
        }
        input => Err(Error::Error(input)),
    }
}

fn small_atom(s: &[u8]) -> Result<(&[u8], Term), Error> {
    match s {
        [len, s @ ..] => {
            let len = *len as usize;
            if len <= s.len() {
                let (string_bytes, s) = s.split_at(len);
                let inner = std::str::from_utf8(string_bytes);

                if let Ok(inner) = inner {
                    Ok((s, Term::Atom(inner)))
                } else {
                    Err(Error::Error(s))
                }
            } else {
                Err(Error::Error(s))
            }
        }
        input => Err(Error::Error(input)),
    }
}

fn small_atom_utf8(s: &[u8]) -> Result<(&[u8], Term), Error> {
    match s {
        [len, s @ ..] => {
            let len = *len as usize;
            if len <= s.len() {
                let (string_bytes, s) = s.split_at(len);
                let inner = std::str::from_utf8(string_bytes);

                if let Ok(inner) = inner {
                    Ok((s, Term::Atom(inner)))
                } else {
                    Err(Error::Error(s))
                }
            } else {
                Err(Error::Error(s))
            }
        }
        input => Err(Error::Error(input)),
    }
}

fn small_tuple(s: &[u8]) -> Result<(&[u8], Term), Error> {
    match s {
        [len, s @ ..] => {
            let len = *len as usize;
            let mut elements = Vec::with_capacity(len);

            let mut s = s;

            for _ in 0..len {
                let (rest, element) = term(s)?;
                s = rest;
                elements.push(element);
            }

            Ok((s, Term::Tuple(elements)))
        }
        input => Err(Error::Error(input)),
    }
}

fn large_tuple(s: &[u8]) -> Result<(&[u8], Term), Error> {
    match s {
        [b1, b2, b3, b4, s @ ..] => {
            let len = u32::from_be_bytes([*b1, *b2, *b3, *b4]) as usize;

            let mut elements = Vec::with_capacity(len);

            let mut s = s;

            for _ in 0..len {
                let (rest, element) = term(s)?;
                s = rest;
                elements.push(element);
            }

            Ok((s, Term::Tuple(elements)))
        }
        input => Err(Error::Error(input)),
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn atom() {
        let string = [131, 100, 0, 5, 104, 101, 108, 108, 111];
        let parsed = parse(&string).unwrap();
        assert_eq!(parsed, Term::Atom("hello"));
    }

    #[test]
    fn atom_utf8() {
        let string = [131, 118, 0, 5, 104, 101, 108, 108, 111];
        let parsed = parse(&string).unwrap();
        assert_eq!(parsed, Term::Atom("hello"));
    }

    #[test]
    fn small_atom() {
        let string = [131, 115, 5, 104, 101, 108, 108, 111];
        let parsed = parse(&string).unwrap();
        assert_eq!(parsed, Term::Atom("hello"));
    }

    #[test]
    fn small_atom_utf8() {
        let string = [131, 119, 5, 104, 101, 108, 108, 111];
        let parsed = parse(&string).unwrap();
        assert_eq!(parsed, Term::Atom("hello"));
    }

    #[test]
    fn string() {
        let string = [131, 109, 0, 0, 0, 5, 104, 101, 108, 108, 111];
        let parsed = parse(&string).unwrap();
        assert_eq!(parsed, Term::Binary("hello"));
    }

    #[test]
    fn empty_map() {
        let empty_map = [131, 116, 0, 0, 0, 0];
        let parsed = parse(&empty_map).unwrap();
        assert_eq!(parsed, Term::Map(BTreeMap::new()));
    }

    #[test]
    fn string_map() {
        let map = [
            131, 116, 0, 0, 0, 1, 109, 0, 0, 0, 5, 104, 101, 108, 108, 111, 109, 0, 0, 0, 5, 116,
            104, 101, 114, 101,
        ];

        let mut btm = BTreeMap::new();
        btm.insert(Term::Binary("hello"), Term::Binary("there"));
        let parsed = parse(&map).unwrap();
        assert_eq!(parsed, Term::Map(btm));
    }

    #[test]
    fn empty_list() {
        let empty_list = [131, 106];
        let parsed = parse(&empty_list).unwrap();
        assert_eq!(parsed, Term::Nil);
    }

    #[test]
    fn list() {
        let list = [
            131, 108, 0, 0, 0, 1, 109, 0, 0, 0, 5, 104, 101, 108, 108, 111, 106,
        ];
        let parsed = parse(&list).unwrap();
        let expected = Term::List(vec![Term::Binary("hello")]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn improper_list() {
        // :erlang.term_to_binary([1 | 2])
        let list = [131, 108, 0, 0, 0, 1, 97, 1, 97, 2];
        let parsed = parse(&list).unwrap();
        let expected = Term::List(vec![Term::SmallInteger(1), Term::SmallInteger(2)]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn small_integer() {
        let small_int = [131, 97, 12];
        let parsed = parse(&small_int).unwrap();
        let expected = Term::SmallInteger(12);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn integer() {
        let int = [131, 98, 255, 255, 255, 244];
        let parsed = parse(&int).unwrap();
        let expected = Term::Integer(-12);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn new_float() {
        let float = [131, 70, 64, 64, 12, 204, 204, 204, 204, 205];
        let parsed = parse(&float).unwrap();
        let expected = Term::Float(32.1.into());
        assert_eq!(parsed, expected);
    }

    #[test]
    fn small_big() {
        // a big number printed from an iex console
        let small_big =
            std::fs::read("fixtures/small_big.etf").expect("could not read small_big.etf fixture");

        let parsed = parse(&small_big).unwrap();

        let expected = Term::BigInt(
            num_bigint::BigInt::from_radix_le(
                num_bigint::Sign::Plus,
                &[
                    199, 113, 28, 199, 171, 237, 49, 63, 73, 243, 92, 107, 122, 5,
                ],
                256,
            )
            .unwrap(),
        );
        assert_eq!(parsed, expected);
    }
    #[test]
    fn large_big() {
        // a really big number printed from an iex console
        let large_big =
            std::fs::read("fixtures/large_big.etf").expect("could not read large_big.etf fixture");

        let parsed = parse(&large_big).unwrap();

        let expected = Term::BigInt(
            num_bigint::BigInt::from_radix_le(
                num_bigint::Sign::Plus,
                &[
                    28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199,
                    113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28,
                    199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113,
                    28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199,
                    113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 171, 73, 10,
                    20, 34, 212, 21, 226, 95, 123, 248, 214, 6, 53, 243, 229, 35, 19, 174, 76, 156,
                    39, 218, 47, 2, 218, 1, 131, 80, 141, 17, 13, 230, 131, 61, 130, 201, 110, 22,
                    160, 204, 187, 230, 8, 227, 198, 134, 77, 167, 137, 116, 53, 70, 144, 160, 12,
                    160, 102, 159, 78, 254, 49, 155, 44, 109, 161, 58, 241, 149, 157, 156, 217, 13,
                    207, 73, 19, 217, 223, 157, 49, 233, 36, 110, 227, 185, 63, 139, 63, 194, 106,
                    174, 247, 236, 207, 91, 155, 181, 121, 152, 222, 251, 92, 87, 56, 160, 64, 208,
                    164, 171, 64, 32, 197, 205, 168, 66, 248, 248, 78, 186, 129, 126, 96, 0, 202,
                    176, 40, 119, 112, 206, 95, 195, 217, 122, 135, 246, 99, 150, 134, 55, 75, 233,
                    113, 130, 86, 248, 96, 29, 20, 168, 118, 55, 194, 254, 217, 225, 204, 14, 45,
                    56, 52, 33, 218, 221, 106, 209, 33, 122, 132, 162, 236, 106, 134, 75, 63, 144,
                    142, 238, 212, 224, 43, 122, 189, 232, 10, 100, 123, 28, 99, 162, 10, 4,
                ],
                256,
            )
            .unwrap(),
        );
        assert_eq!(parsed, expected);
    }

    #[test]
    fn heterogeneous_small_map() {
        // {:a 73 :b 8248 :c "hello" :d :ok :e 99}
        let small_map = [
            131, 116, 0, 0, 0, 5, 100, 0, 1, 97, 97, 73, 100, 0, 1, 98, 98, 0, 0, 32, 56, 100, 0,
            1, 99, 109, 0, 0, 0, 5, 104, 101, 108, 108, 111, 100, 0, 1, 100, 100, 0, 2, 111, 107,
            100, 0, 1, 101, 97, 99,
        ];

        let parsed = parse(&small_map).unwrap();
        let mut map = BTreeMap::new();
        map.insert(Term::Atom("a"), Term::SmallInteger(73));
        map.insert(Term::Atom("b"), Term::Integer(8248));
        map.insert(Term::Atom("c"), Term::Binary("hello"));
        map.insert(Term::Atom("d"), Term::Atom("ok"));
        map.insert(Term::Atom("e"), Term::SmallInteger(99));
        let expected = Term::Map(map);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn small_tuple() {
        let small_tuple = [
            131, 104, 5, 100, 0, 1, 49, 100, 0, 1, 50, 100, 0, 1, 51, 100, 0, 1, 52, 100, 0, 1, 53,
        ];
        let parsed = parse(&small_tuple).unwrap();
        assert_eq!(
            parsed,
            Term::Tuple(vec![
                Term::Atom("1"),
                Term::Atom("2"),
                Term::Atom("3"),
                Term::Atom("4"),
                Term::Atom("5"),
            ])
        );
    }

    #[test]
    fn large_tuple() {
        let large_tuple = std::fs::read("fixtures/large_tuple.etf")
            .expect("could not read large_tuple.etf fixture");
        let parsed = parse(&large_tuple).unwrap();
        let mut strs = Vec::with_capacity(256);
        let mut elements = Vec::with_capacity(256);
        for i in 1..=256 {
            let s = i.to_string();
            strs.push(s);
        }
        for i in 0..256 {
            elements.push(Term::Atom(&strs[i]));
        }

        assert_eq!(parsed, Term::Tuple(elements));
    }
}