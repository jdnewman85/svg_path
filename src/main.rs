//M 100 100 L 300 100 L 200 300 z

use std::error::Error;
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
type CurveToCoordinateSequence = Vec<CoordinatePairTriplet>;
type SmoothCurveToCoordinateSequence = Vec<CoordinatePairDouble>;
type QuadraticBezierCurveToCoordinateSequence = Vec<CoordinatePairDouble>;
type Flag = bool;
type EllipticalArcArgument = (Number, Number, Number, Flag, Flag, CoordinatePair);
type EllipticalArcArgumentSequence = Vec<EllipticalArcArgument>;
type SvgPath = Vec<SvgWord>;
#[derive(Debug)]
enum SvgWord {
    MoveTo(CoordinatePairSequence),
    ClosePath,
    LineTo(CoordinatePairSequence),
    HorizontalLineTo(CoordinateSequence),
    VerticalLineTo(CoordinateSequence),
    CurveTo(CurveToCoordinateSequence),
    SmoothCurveTo(SmoothCurveToCoordinateSequence),
    QuadraticBezierCurveTo(QuadraticBezierCurveToCoordinateSequence),
    SmoothQuadraticBezierCurveTo(CoordinatePairSequence),
    EllipticalArc(EllipticalArcArgumentSequence),
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

//TODO Could factor some commands together into a factory
fn move_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, coord_pairs) = coordinate_pair_sequence_command_factory(|c| c == 'M' || c == 'm', input)?;
    Ok((input, SvgWord::MoveTo(coord_pairs)))
}

fn line_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, coord_pairs) = coordinate_pair_sequence_command_factory(|c| c == 'L' || c == 'l', input)?;
    Ok((input, SvgWord::LineTo(coord_pairs)))
}

fn close_path(input: &str) -> IResult<&str, SvgWord> {
    let (input, _) = satisfy(|c| c == 'Z' || c == 'z')(input)?;
    Ok((input, SvgWord::ClosePath))
}

fn horizontal_line_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, coords) = coordinate_sequence_command_factory(|c| c == 'H' || c == 'h', input)?;
    Ok((input, SvgWord::HorizontalLineTo(coords)))
}

fn vertical_line_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, coords) = coordinate_sequence_command_factory(|c| c == 'V' || c == 'v', input)?;
    Ok((input, SvgWord::VerticalLineTo(coords)))
}

fn coordinate_sequence_command_factory(
    is_cmd: impl Fn(char) -> bool,
    input: &str
) -> IResult<&str, CoordinateSequence> {
    let (input, coords) =
    preceded(
        tuple((
            satisfy(is_cmd),
            take_while(is_wsp),
        )),
        coordinate_sequence,
    )(input)?;
    Ok((input, coords))
}

fn coordinate_pair_sequence_command_factory(
    is_cmd: impl Fn(char) -> bool,
    input: &str
) -> IResult<&str, CoordinatePairSequence> {
    let (input, coords) =
    preceded(
        tuple((
            satisfy(is_cmd),
            take_while(is_wsp),
        )),
        coordinate_pair_sequence,
    )(input)?;
    Ok((input, coords))
}

fn curve_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, coords) =
    preceded(
        tuple((
            satisfy(|c| c == 'C' || c == 'c'),
            take_while(is_wsp),
        )),
        curve_to_coordinate_sequence,
    )(input)?;
    Ok((input, SvgWord::CurveTo(coords)))
}

fn curve_to_coordinate_sequence(input: &str) -> IResult<&str, CurveToCoordinateSequence> {
    many1(
        terminated(coordinate_pair_triplet, opt(comma_wsp))
    )(input)
}

fn smooth_curve_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, coords) =
    preceded(
        tuple((
            satisfy(|c| c == 'S' || c == 's'),
            take_while(is_wsp),
        )),
        smooth_curve_to_coordinate_sequence,
    )(input)?;
    Ok((input, SvgWord::SmoothCurveTo(coords)))
}

fn smooth_curve_to_coordinate_sequence(input: &str) -> IResult<&str, SmoothCurveToCoordinateSequence> {
    many1(
        terminated(coordinate_pair_double, opt(comma_wsp))
    )(input)
}

fn quadratic_bezier_curve_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, coords) =
    preceded(
        tuple((
            satisfy(|c| c == 'Q' || c == 'q'),
            take_while(is_wsp),
        )),
        quadratic_bezier_curve_to_coordinate_sequence,
    )(input)?;
    Ok((input, SvgWord::QuadraticBezierCurveTo(coords)))
}

fn quadratic_bezier_curve_to_coordinate_sequence(input: &str) -> IResult<&str, QuadraticBezierCurveToCoordinateSequence> {
    many1(
        terminated(coordinate_pair_double, opt(comma_wsp))
    )(input)
}

fn smooth_quadratic_bezier_curve_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, coords) =
    preceded(
        tuple((
            satisfy(|c| c == 'T' || c == 't'),
            take_while(is_wsp),
        )),
        coordinate_pair_sequence,
    )(input)?;
    Ok((input, SvgWord::SmoothQuadraticBezierCurveTo(coords)))
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
    let (input, args) =
    preceded(
        tuple((
            satisfy(|c| c == 'A' || c == 'a'),
            take_while(is_wsp),
        )),
        elliptical_arc_argument_sequence,
    )(input)?;
    Ok((input, SvgWord::EllipticalArc(args)))
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
    println!("Hi");

    let path = "M140 20C73 20 20 74 20 140c0 135 136 170 228 303 88-132 229-173 229-303 0-66-54-120-120-120-48 0-90 28-109 69-19-41-60-69-108-69z";
    let (remaining, path) = svg_path(path)?;

    dbg!(remaining);
    dbg!(path);

    Ok(())

}
