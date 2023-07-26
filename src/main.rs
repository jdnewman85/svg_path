//M 100 100 L 300 100 L 200 300 z

use std::error::Error;
use std::fmt::{Display, self};
use nom::multi::{many1, many0};
use nom::bytes::complete::{take_while, take_while1};
use nom::combinator::{opt, recognize};
use nom::branch::alt;

use nom::number::complete::float;
use nom::sequence::{separated_pair, terminated, preceded};
use nom::{IResult, character::complete::{char, satisfy}, sequence::tuple};

type Number = f32;
type CoordinatePair = (Number, Number);
type CoordinateSequence = Vec<Number>;
type CoordinatePairSequence = Vec<CoordinatePair>;
type CoordinatePairDouble = (CoordinatePair, CoordinatePair);
type CoordinatePairTriplet = (CoordinatePair, CoordinatePair, CoordinatePair);
type CoordinatePairDoubleSequence = Vec<CoordinatePairDouble>;
type CurveToCoordinateSequence = Vec<CoordinatePairTriplet>;
type SmoothCurveToCoordinateSequence = CoordinatePairDoubleSequence;
type QuadraticBezierCurveToCoordinateSequence = CoordinatePairDoubleSequence;
type Flag = bool;
type EllipticalArcArgument = (Number, Number, Number, Flag, Flag, CoordinatePair);
type EllipticalArcArgumentSequence = Vec<EllipticalArcArgument>;
type SvgPath = Vec<SvgWord>;

type IsRelative = bool;
#[derive(Debug)]
enum SvgWord {
    MoveTo(IsRelative, CoordinatePairSequence),
    ClosePath(IsRelative),
    LineTo(IsRelative, CoordinatePairSequence),
    HorizontalLineTo(IsRelative, CoordinateSequence),
    VerticalLineTo(IsRelative, CoordinateSequence),
    CurveTo(IsRelative, CurveToCoordinateSequence),
    SmoothCurveTo(IsRelative, SmoothCurveToCoordinateSequence),
    QuadraticBezierCurveTo(IsRelative, QuadraticBezierCurveToCoordinateSequence),
    SmoothQuadraticBezierCurveTo(IsRelative, CoordinatePairSequence),
    EllipticalArc(IsRelative, EllipticalArcArgumentSequence),
}

//TODO Abs/Rel
//TODO Source letter from method of self
impl Display for SvgWord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.command_char(), self.command_args_string())
    }
}

impl SvgWord {
    fn command_char(&self) -> char {
        match self {
            Self::MoveTo(is_rel, _coord_pairs) => letter_abs_rel('M', *is_rel),
            Self::ClosePath(is_rel) => letter_abs_rel('Z', *is_rel),
            Self::LineTo(is_rel, _coord_pairs) => letter_abs_rel('L', *is_rel),
            Self::HorizontalLineTo(is_rel, _coords) => letter_abs_rel('H', *is_rel),
            Self::VerticalLineTo(is_rel, _coords) => letter_abs_rel('V', *is_rel),
            Self::CurveTo(is_rel, _coord_triplets) => letter_abs_rel('C', *is_rel),
            Self::SmoothCurveTo(is_rel, _coord_doubles) => letter_abs_rel('S', *is_rel),
            Self::QuadraticBezierCurveTo(is_rel, _coord_doubles) => letter_abs_rel('Q', *is_rel),
            Self::SmoothQuadraticBezierCurveTo(is_rel, _coord_pairs) => letter_abs_rel('T', *is_rel),
            Self::EllipticalArc(is_rel, _arc_args) => letter_abs_rel('A', *is_rel),
        }
    }
    fn command_args_string(&self) -> String {
        match self {
            Self::MoveTo(_is_rel, coord_pairs) => CoordinatePairSequenceStruct(coord_pairs).to_string(),
            Self::ClosePath(_is_rel) => "".to_string(),
            Self::LineTo(_is_rel, coord_pairs) => CoordinatePairSequenceStruct(coord_pairs).to_string(),
            Self::HorizontalLineTo(_is_rel, coords) => CoordinateSequenceStruct(coords).to_string(),
            Self::VerticalLineTo(_is_rel, coords) => CoordinateSequenceStruct(coords).to_string(),
            Self::CurveTo(_is_rel, coord_triplets) => CurveToCoordinateSequenceStruct(coord_triplets).to_string(),
            Self::SmoothCurveTo(_is_rel, coord_doubles) => CoordinatePairDoubleSequenceStruct(coord_doubles).to_string(),
            Self::QuadraticBezierCurveTo(_is_rel, coord_doubles) => CoordinatePairDoubleSequenceStruct(coord_doubles).to_string(),
            Self::SmoothQuadraticBezierCurveTo(_is_rel, coord_pairs) => CoordinatePairSequenceStruct(coord_pairs).to_string(),
            Self::EllipticalArc(_is_rel, arc_args) => EllipticalArcArgumentSequenceStuct(arc_args).to_string(),
        }
    }
}

impl SvgWord {
    //TODO Replace with vec#s and .scale() calls to those
    fn scale(&mut self, s: f32) {
        match self {
            Self::MoveTo(_is_rel, coord_pairs) =>
                coord_pairs.iter_mut().for_each(|(x, y)| {
                    *x *= s;
                    *y *= s;
                }),
            Self::ClosePath(_is_rel) => {},
            Self::LineTo(_is_rel, coord_pairs) =>
                coord_pairs.iter_mut().for_each(|(x, y)| {
                    *x *= s;
                    *y *= s;
                }),
            Self::HorizontalLineTo(_is_rel, coords) =>
                coords.iter_mut().for_each(|x| *x *= s),
            Self::VerticalLineTo(_is_rel, coords) =>
                coords.iter_mut().for_each(|y| *y *= s),
            Self::CurveTo(_is_rel, coord_triplets) =>
                coord_triplets.iter_mut().for_each(|((x1, y1), (x2, y2), (x, y))| {
                    *x1 *= s;
                    *y1 *= s;
                    *x2 *= s;
                    *y2 *= s;
                     *x *= s;
                     *y *= s;
                }),
            Self::SmoothCurveTo(_is_rel, coord_doubles) =>
                coord_doubles.iter_mut().for_each(|((x2, y2), (x, y))| {
                    *x2 *= s;
                    *y2 *= s;
                     *x *= s;
                     *y *= s;
                }),
            Self::QuadraticBezierCurveTo(_is_rel, coord_doubles) =>
                coord_doubles.iter_mut().for_each(|((x1, y1), (x, y))| {
                    *x1 *= s;
                    *y1 *= s;
                     *x *= s;
                     *y *= s;
                }),
            Self::SmoothQuadraticBezierCurveTo(_is_rel, coord_pairs) =>
                coord_pairs.iter_mut().for_each(|(x, y)| {
                    *x *= s;
                    *y *= s;
                }),
            Self::EllipticalArc(_is_rel, arc_args) =>
                arc_args.iter_mut().for_each(|(rx, ry, _x_rotation, _large_arc, _sweep, (x, y))| {
                    *rx *= s;
                    *ry *= s;
                     *x *= s;
                     *y *= s;
                }),
        }
    }
}

fn letter_abs_rel(c: char, is_rel: bool) -> char {
    match is_rel {
        true => c.to_ascii_lowercase(),
        false => c.to_ascii_uppercase(),
    }
}

//TODO Rename these structs?
//TODO Appropriate? Maybe a different trait?
struct SvgPathStruct<'a>(&'a mut SvgPath);
impl Display for SvgPathStruct<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
            self.0.iter().map(|word| word.to_string()).collect::<String>()
        )
    }
}

impl SvgPathStruct<'_> {
    fn scale(&mut self, s: f32) {
        for word in self.0.iter_mut() {
            word.scale(s);
        }
    }
}

struct CoordinatePairSequenceStruct<'a>(&'a CoordinatePairSequence);
impl Display for CoordinatePairSequenceStruct<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
            self.0.iter()
                .map(|(x, y)| format!("{x},{y}"))
                .collect::<Vec<_>>().join(" ")
        )
    }
}

struct CoordinateSequenceStruct<'a>(&'a CoordinateSequence);
impl Display for CoordinateSequenceStruct<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
            self.0.iter()
                .map(|c| format!("{c}"))
                .collect::<Vec<_>>().join(" ")
        )
    }
}

struct CurveToCoordinateSequenceStruct<'a>(&'a CurveToCoordinateSequence);
impl Display for CurveToCoordinateSequenceStruct<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
            self.0.iter()
                .map(|((x1, y1), (x2, y2), (x, y))| format!("{x1},{y1} {x2},{y2} {x},{y}"))
                .collect::<Vec<_>>().join(" ")
        )
    }
}

struct CoordinatePairDoubleSequenceStruct<'a>(&'a CoordinatePairDoubleSequence);
impl Display for CoordinatePairDoubleSequenceStruct<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
            self.0.iter()
                .map(|((x1, y1), (x, y))| format!("{x1},{y1} {x},{y}"))
                .collect::<Vec<_>>().join(" ")
        )
    }
}

struct EllipticalArcArgumentSequenceStuct<'a>(&'a EllipticalArcArgumentSequence);
impl Display for EllipticalArcArgumentSequenceStuct<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
            self.0.iter()
                .map(|(rx, ry, x_rotation, large_arc, sweep, (x, y))| {
                    let large_arc = *large_arc as i32;
                    let sweep = *sweep as i32;
                    format!("{rx},{ry} {x_rotation} {large_arc} {sweep} {x},{y}")
                })
                .collect::<Vec<_>>().join(" ")
        )
    }
}

//TODO Make "correct", or have alternative correct version
fn svg_path(input: &str) -> IResult<&str, SvgPath> {
    preceded(
        take_while(is_wsp),
        many0(drawto_command)
    )(input)
}

fn drawto_command(input: &str) -> IResult<&str, SvgWord> {
    alt((
        move_to,
        line_to,
        close_path,
        horizontal_line_to,
        vertical_line_to,
        curve_to,
        smooth_curve_to,
        quadratic_bezier_curve_to,
        smooth_quadratic_bezier_curve_to,
        elliptical_arc,
    ))(input)
}

fn svg_word<O>(input: &str, cmd_letter: char, arg_parser: fn(&str)->IResult<&str, O>) -> IResult<&str, (bool, O)> {
    let (input, (c, coords)) =
    separated_pair(
        satisfy(|c| c.to_ascii_uppercase() == cmd_letter),
        take_while(is_wsp),
        arg_parser,
    )(input)?;
    let is_rel = c.is_ascii_lowercase();
    Ok((input,(is_rel, coords)))
}


//TODO Could factor some commands together into a factory
fn move_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, (is_rel, args)) = svg_word(input, 'M', coordinate_pair_sequence)?;
    Ok((input, SvgWord::MoveTo(is_rel, args)))
}

fn line_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, (is_rel, args)) = svg_word(input, 'L', coordinate_pair_sequence)?;
    Ok((input, SvgWord::LineTo(is_rel, args)))
}

fn close_path(input: &str) -> IResult<&str, SvgWord> {
    let (input, c) = satisfy(|c| c == 'Z' || c == 'z')(input)?;
    let is_rel = c.is_ascii_lowercase();
    Ok((input, SvgWord::ClosePath(is_rel)))
}

fn horizontal_line_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, (is_rel, args)) = svg_word(input, 'H', coordinate_sequence)?;
    Ok((input, SvgWord::HorizontalLineTo(is_rel, args)))
}

fn vertical_line_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, (is_rel, args)) = svg_word(input, 'V', coordinate_sequence)?;
    Ok((input, SvgWord::VerticalLineTo(is_rel, args)))
}

fn curve_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, (is_rel, args)) = svg_word(input, 'C', curve_to_coordinate_sequence)?;
    Ok((input, SvgWord::CurveTo(is_rel, args)))
}

fn curve_to_coordinate_sequence(input: &str) -> IResult<&str, CurveToCoordinateSequence> {
    many1(
        terminated(coordinate_pair_triplet, opt(comma_wsp))
    )(input)
}

fn smooth_curve_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, (is_rel, args)) = svg_word(input, 'S', smooth_curve_to_coordinate_sequence)?;
    Ok((input, SvgWord::SmoothCurveTo(is_rel, args)))
}

fn smooth_curve_to_coordinate_sequence(input: &str) -> IResult<&str, SmoothCurveToCoordinateSequence> {
    many1(
        terminated(
            coordinate_pair_double,
            opt(comma_wsp)
        )
    )(input)
}

fn quadratic_bezier_curve_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, (is_rel, args)) = svg_word(input, 'Q', quadratic_bezier_curve_to_coordinate_sequence)?;
    Ok((input, SvgWord::QuadraticBezierCurveTo(is_rel, args)))
}

fn quadratic_bezier_curve_to_coordinate_sequence(input: &str) -> IResult<&str, QuadraticBezierCurveToCoordinateSequence> {
    many1(
        terminated(
            coordinate_pair_double,
            opt(comma_wsp)
        )
    )(input)
}

fn smooth_quadratic_bezier_curve_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, (is_rel, args)) = svg_word(input, 'T', coordinate_pair_sequence)?;
    Ok((input, SvgWord::SmoothQuadraticBezierCurveTo(is_rel, args)))
}

fn elliptical_arc_argument(input: &str) -> IResult<&str, EllipticalArcArgument> {
    tuple((
        float,
        float,
        float,
        flag,
        flag,
        coordinate_pair,
    ))(input)
}

fn elliptical_arc_argument_sequence(input: &str) -> IResult<&str, EllipticalArcArgumentSequence> {
    many1(
        terminated(elliptical_arc_argument, opt(comma_wsp))
    )(input)
}

fn elliptical_arc(input: &str) -> IResult<&str, SvgWord> {
    let (input, (is_rel, args)) = svg_word(input, 'A', elliptical_arc_argument_sequence)?;
    Ok((input, SvgWord::EllipticalArc(is_rel, args)))
}

fn flag(input: &str) -> IResult<&str, bool> {
    let (input, c) = satisfy(is_flag)(input)?;
    let value = match c {
        '0' => false,
        '1' => true,
        _ => unreachable!(),
    };

    Ok((input, value))
}

fn coordinate_pair_double(input: &str) -> IResult<&str, CoordinatePairDouble> {
    tuple((
        coordinate_pair,
        preceded(
            take_while(is_wsp),
            coordinate_pair
        ),
    ))(input)
}

fn coordinate_pair_triplet(input: &str) -> IResult<&str, CoordinatePairTriplet> {
    tuple((
        coordinate_pair,
        preceded(
            take_while(is_wsp),
            coordinate_pair
        ),
        preceded(
            take_while(is_wsp),
            coordinate_pair
        ),
    ))(input)
}

fn coordinate_pair_sequence(input: &str) -> IResult<&str, CoordinatePairSequence> {
    many1(
        terminated(coordinate_pair, opt(comma_wsp))
    )(input)
}

fn coordinate_sequence(input: &str) -> IResult<&str, CoordinateSequence> {
    many1(
        terminated(coordinate, opt(comma_wsp))
    )(input)
}

fn coordinate_pair(input: &str) -> IResult<&str, CoordinatePair> {
    separated_pair(
        coordinate,
        opt(comma_wsp),
        coordinate,
    )(input)
}

fn coordinate(input: &str) -> IResult<&str, Number> {
    float(input)
}

fn is_wsp(c: char) -> bool {
    match c {
        '\x09' | '\x20' | '\x0A' | '\x0C' | '\x0D' => true,
        _ => false,
    }
}

fn comma_wsp(input: &str) -> IResult<&str, &str> {
    alt((
        recognize(tuple((
            take_while1(is_wsp),
            opt(char(',')),
            take_while(is_wsp),
        ))),
        recognize(tuple((
            char(','),
            take_while(is_wsp),
        ))),
    )) (input)
}

fn is_flag(c: char) -> bool {
    match c {
        '0' | '1' => true,
        _ => false,
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let path = "M -23.69,-49.89 C -38.31,-49.89 -49.89,-37.14 -49.89,-21.57 C -49.89,10.32 -20.2,18.56 -0.1065,50 C 19.11,18.8 49.9,9.144 49.9,-21.57 C 49.9,-37.14 38.1,-49.89 23.69,-49.89 C 13.21,-49.89 4.044,-43.28 -0.1065,-33.6 C -4.256,-43.28 -13.21,-49.89 -23.69,-49.89 Z";
    println!("Original path:\n\t{path}");


    let (_remaining, mut path) = svg_path(path)?;
    let mut path = SvgPathStruct(&mut path);
    path.scale(2.0);

    println!("As new path:\n\t{}", path);

    Ok(())

}
