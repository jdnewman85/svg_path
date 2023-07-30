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

use glam::Vec2;

type Number = f32;
type CoordinatePair = Vec2;
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

#[derive(Debug)]
struct BoundingBox {
    min: CoordinatePair,
    max: CoordinatePair,
}

impl BoundingBox {
    fn merge(&self, bb: BoundingBox) -> BoundingBox {
        let min_x = self.min.x.min(bb.min.x);
        let min_y = self.min.y.min(bb.min.y);
        let max_x = self.max.x.max(bb.max.x);
        let max_y = self.max.y.max(bb.max.y);
        BoundingBox {
            min: (min_x, min_y).into(),
            max: (max_x, max_y).into(),
        }
    }
}

impl From<CoordinatePair> for BoundingBox {
    fn from(p: CoordinatePair) -> Self {
        BoundingBox { min: p, max: p }
    }
}
impl From<CoordinatePairDouble> for BoundingBox {
    fn from(l: CoordinatePairDouble) -> Self {
        BoundingBox::from(l.0).merge(l.1.into())
    }
}

#[derive(Debug)]
enum SvgWordEndpoint {
    CoordinatePair(CoordinatePair),
    StartOfPath,
}

#[derive(Debug)]
enum SvgWordKind {
    MoveTo(CoordinatePairSequence),
    LineTo(CoordinatePairSequence),
    HorizontalLineTo(CoordinateSequence),
    VerticalLineTo(CoordinateSequence),
    CurveTo(CurveToCoordinateSequence), //cubic Bezier
    SmoothCurveTo(SmoothCurveToCoordinateSequence),
    QuadraticBezierCurveTo(QuadraticBezierCurveToCoordinateSequence), //quad bezier
    SmoothQuadraticBezierCurveTo(CoordinatePairSequence),
    EllipticalArc(EllipticalArcArgumentSequence), //
    ClosePath,
}

struct SvgWord {
    is_relative: bool,
    word: SvgWordKind,
}

impl Display for SvgWord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.command_char(), self.command_args_string())
    }
}

impl SvgWord {
    fn command_char_abs(&self) -> char {
        match self.word {
            SvgWordKind::MoveTo(_) => 'M',
            SvgWordKind::LineTo(_) => 'L',
            SvgWordKind::HorizontalLineTo(_) => 'H',
            SvgWordKind::VerticalLineTo(_) => 'V',
            SvgWordKind::CurveTo(_) => 'C',
            SvgWordKind::SmoothCurveTo(_) => 'S',
            SvgWordKind::QuadraticBezierCurveTo(_) => 'Q',
            SvgWordKind::SmoothQuadraticBezierCurveTo(_) => 'T',
            SvgWordKind::EllipticalArc(_) => 'A',
            SvgWordKind::ClosePath => 'Z',
        }
    }
    fn command_char_rel(&self) -> char {
        self.command_char_abs().to_ascii_lowercase()
    }
    fn command_char(&self) -> char {
        match self.is_relative {
            true => self.command_char_rel(),
            false => self.command_char_abs(),
        }
    }
    fn command_args_string(&self) -> String {
        match &self.word {
            SvgWordKind::MoveTo(coord_pairs) => CoordinatePairSequenceStruct(&coord_pairs).to_string(),
            SvgWordKind::LineTo(coord_pairs) => CoordinatePairSequenceStruct(&coord_pairs).to_string(),
            SvgWordKind::HorizontalLineTo(coords) => CoordinateSequenceStruct(&coords).to_string(),
            SvgWordKind::VerticalLineTo(coords) => CoordinateSequenceStruct(&coords).to_string(),
            SvgWordKind::CurveTo(coord_triplets) => CurveToCoordinateSequenceStruct(&coord_triplets).to_string(),
            SvgWordKind::SmoothCurveTo(coord_doubles) => CoordinatePairDoubleSequenceStruct(&coord_doubles).to_string(),
            SvgWordKind::QuadraticBezierCurveTo(coord_doubles) => CoordinatePairDoubleSequenceStruct(&coord_doubles).to_string(),
            SvgWordKind::SmoothQuadraticBezierCurveTo(coord_pairs) => CoordinatePairSequenceStruct(&coord_pairs).to_string(),
            SvgWordKind::EllipticalArc(arc_args) => EllipticalArcArgumentSequenceStuct(&arc_args).to_string(),
            SvgWordKind::ClosePath => "".to_string(),
        }
    }
    fn bounding_box(&self, start: CoordinatePair) -> BoundingBox {
        let init_bb = BoundingBox{ min: start, max: start };
        match &self.word {
            SvgWordKind::MoveTo(coord_pairs) => {
                coord_pairs.iter().fold(
                    (init_bb, start),
                    |(acc_bb, current_position), point| {
                        let maybe_offset_point = self.maybe_offset(point, current_position);
                        (acc_bb.merge(maybe_offset_point.into()), maybe_offset_point)
                    }
                ).0
            },
            SvgWordKind::LineTo(coord_pairs) => {
                coord_pairs.iter().fold(
                    (init_bb, start),
                    |(acc_bb, current_position), point| {
                        let maybe_offset_point = self.maybe_offset(point, current_position);
                        (acc_bb.merge(maybe_offset_point.into()), maybe_offset_point)
                    }
                ).0
            },
            SvgWordKind::HorizontalLineTo(coords) =>
                coords.iter().fold(
                    (init_bb, start),
                    |(acc_bb, current_position), x|
                    (
                        acc_bb.merge(
                            BoundingBox::from(
                                self.maybe_offset(
                                    &(*x, start.y).into(),
                                    current_position
                                )
                            )
                        ),
                        (*x, start.y).into()
                    )
                ).0,
            SvgWordKind::VerticalLineTo(coords) =>
                coords.iter().fold(
                    (init_bb, start),
                    |(acc_bb, current_position), y|
                    (
                        acc_bb.merge(
                            BoundingBox::from(
                                self.maybe_offset(
                                    &(start.x, *y).into(),
                                    current_position
                                )
                            )
                        ),
                        (start.x, *y).into()
                    )
                ).0,
            SvgWordKind::CurveTo(_coord_triplets) => todo!(),
            SvgWordKind::SmoothCurveTo(_coord_doubles) => todo!(),
            SvgWordKind::QuadraticBezierCurveTo(_coord_doubles) => todo!(),
            SvgWordKind::SmoothQuadraticBezierCurveTo(_coord_pairs) => todo!(),
            SvgWordKind::EllipticalArc(_arc_args) => todo!(),
            SvgWordKind::ClosePath => {
                //TODO: Re-evaluate, should we somehow try to include the start of the path? It
                //shouldn't change the bounding box.
                start.into()
            },
        }
    }

    fn maybe_offset(&self, point: &CoordinatePair, current_position: CoordinatePair) -> CoordinatePair {
        match self.is_relative {
//            true => (current_position.x + point.x, current_position.y + point.y).into(),
            true => current_position+*point,
            false => *point,
        }
    }
    //TODO DRY?
    fn endpoint(&self, start: CoordinatePair) -> SvgWordEndpoint {
        match &self.word {
            SvgWordKind::MoveTo(coord_pairs) => {
                SvgWordEndpoint::CoordinatePair(
                    match self.is_relative {
                        true => coord_pairs.iter().fold(start, |acc, point| acc + *point),
                        false => *coord_pairs.iter().last().unwrap_or(&start),
                    }
                )
            },
            SvgWordKind::LineTo(coord_pairs) => {
                SvgWordEndpoint::CoordinatePair(
                    match self.is_relative {
                        true => coord_pairs.iter().fold(start, |acc, point| acc + *point),
                        false => *coord_pairs.iter().last().unwrap_or(&start),
                    }
                )
            },
            SvgWordKind::HorizontalLineTo(coords) => {
                SvgWordEndpoint::CoordinatePair(
                    match self.is_relative {
                        true => coords.iter().fold(start, |acc, x| (acc.x+x, start.y).into()),
                        false => (*coords.iter().last().unwrap_or(&start.x), start.y).into(),
                    }
                )
            },
            SvgWordKind::VerticalLineTo(coords) => {
                SvgWordEndpoint::CoordinatePair(
                    match self.is_relative {
                        true => coords.iter().fold(start, |acc, y| (start.x, acc.y+y).into()),
                        false => (start.x, *coords.iter().last().unwrap_or(&start.y)).into(),
                    }
                )
            },
            SvgWordKind::CurveTo(coord_triplets) => {
                SvgWordEndpoint::CoordinatePair(
                    match self.is_relative {
                        true => coord_triplets.iter().map(|coord_triplet| coord_triplet.2).fold(start, |acc, point| acc+point),
                        false => {
                            let last_triplet = coord_triplets.iter().last();
                            match last_triplet {
                                Some((_, _, end)) => *end,
                                None => start,
                            }
                        },
                    }
                )
            },
            SvgWordKind::SmoothCurveTo(coord_doubles) => {
                SvgWordEndpoint::CoordinatePair(
                    match self.is_relative {
                        true => coord_doubles.iter().map(|coord_double| coord_double.1).fold(start, |acc, point| acc+point),
                        false => {
                            let last_double = coord_doubles.iter().last();
                            match last_double {
                                Some((_, end)) => *end,
                                None => start,
                            }
                        },
                    }
                )
            },
            SvgWordKind::QuadraticBezierCurveTo(coord_doubles) => {
                SvgWordEndpoint::CoordinatePair(
                    match self.is_relative {
                        true => coord_doubles.iter().map(|coord_double| coord_double.1).fold(start, |acc, point| acc+point),
                        false => {
                            let last_double = coord_doubles.iter().last();
                            match last_double {
                                Some((_, end)) => *end,
                                None => start,
                            }
                        },
                    }
                )
            },
            SvgWordKind::SmoothQuadraticBezierCurveTo(coord_pairs) => {
                SvgWordEndpoint::CoordinatePair(
                    match self.is_relative {
                        true => coord_pairs.iter().fold(start, |acc, point| acc + *point),
                        false => *coord_pairs.iter().last().unwrap_or(&start),
                    }
                )
            },
            SvgWordKind::EllipticalArc(arc_args) => {
                SvgWordEndpoint::CoordinatePair(
                    match self.is_relative {
                        true => arc_args.iter().map(|arc_arg| arc_arg.5).fold(start, |acc, point| acc+point),
                        false => {
                            let last_args = arc_args.iter().last();
                            match last_args {
                                Some((_, _, _, _, _, end)) => *end,
                                None => start,
                            }
                        },
                    }
                )
            },
            SvgWordKind::ClosePath => SvgWordEndpoint::StartOfPath,
        }
    }
}

impl SvgWord {
    //TODO Replace with vec#s and .scale() calls to those
    fn scale(&mut self, s: f32) {
        match &mut self.word {
            SvgWordKind::MoveTo(ref mut coord_pairs) =>
                coord_pairs.iter_mut().for_each(|coord_pair| {
                    coord_pair.x *= s;
                    coord_pair.y *= s;
                }),
            SvgWordKind::LineTo(ref mut coord_pairs) =>
                coord_pairs.iter_mut().for_each(|coord_pair| {
                    coord_pair.x *= s;
                    coord_pair.y *= s;
                }),
            SvgWordKind::HorizontalLineTo(ref mut coords) =>
                coords.iter_mut().for_each(|x| *x *= s),
            SvgWordKind::VerticalLineTo(ref mut coords) =>
                coords.iter_mut().for_each(|y| *y *= s),
            SvgWordKind::CurveTo(ref mut coord_triplets) =>
                coord_triplets.iter_mut().for_each(|(c1, c2, end)| {
                    *c1 *= s;
                    *c2 *= s;
                    *end *= s;
                }),
            SvgWordKind::SmoothCurveTo(ref mut coord_doubles) =>
                coord_doubles.iter_mut().for_each(|(c1, end)| {
                    *c1 *= s;
                    *end *= s;
                }),
            SvgWordKind::QuadraticBezierCurveTo(ref mut coord_doubles) =>
                coord_doubles.iter_mut().for_each(|(c1, end)| {
                    *c1 *= s;
                    *end *= s;
                }),
            SvgWordKind::SmoothQuadraticBezierCurveTo(ref mut coord_pairs) =>
                coord_pairs.iter_mut().for_each(|end| {
                    *end *= s;
                }),
            SvgWordKind::EllipticalArc(ref mut arc_args) =>
                arc_args.iter_mut().for_each(|(rx, ry, _x_rotation, _large_arc, _sweep, end)| {
                    *rx *= s;
                    *ry *= s;
                     *end *= s;
                }),
            SvgWordKind::ClosePath => {},
        }
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
    fn bounding_box(&self, start: CoordinatePair) -> BoundingBox {
        let init_bb = BoundingBox{ min: start, max: start };
        self.0.iter().fold((init_bb, start), |(acc_bb, current_position), word| {
            let acc = acc_bb.merge(word.bounding_box(current_position));
            let word_endpoint = word.endpoint(current_position);
            let endpoint = match word_endpoint {
                SvgWordEndpoint::CoordinatePair(cp) => cp,
                SvgWordEndpoint::StartOfPath => start,
            };
            (acc, endpoint)
        }).0
    }
    fn endpoint(&self, start: CoordinatePair) -> CoordinatePair {
        self.0.iter().fold(start, |acc, word| {
            let word_endpoint = word.endpoint(acc);
            match word_endpoint {
                SvgWordEndpoint::CoordinatePair(cp) => cp,
                SvgWordEndpoint::StartOfPath => start,
            }
        })
    }
}

struct CoordinatePairSequenceStruct<'a>(&'a CoordinatePairSequence);
impl Display for CoordinatePairSequenceStruct<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
            self.0.iter()
                .map(|v| format!("{},{}", v.x, v.y))
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
                .map(|(c1, c2, end)| format!("{},{} {},{} {},{}", c1.x, c1.y, c2.x, c2.y, end.x, end.y))
                .collect::<Vec<_>>().join(" ")
        )
    }
}

struct CoordinatePairDoubleSequenceStruct<'a>(&'a CoordinatePairDoubleSequence);
impl Display for CoordinatePairDoubleSequenceStruct<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
            self.0.iter()
                .map(|(c1, end)| format!("{},{} {},{}", c1.x, c1.y, end.x, end.y))
                .collect::<Vec<_>>().join(" ")
        )
    }
}

struct EllipticalArcArgumentSequenceStuct<'a>(&'a EllipticalArcArgumentSequence);
impl Display for EllipticalArcArgumentSequenceStuct<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
            self.0.iter()
                .map(|(rx, ry, x_rotation, large_arc, sweep, end)| {
                    let large_arc = *large_arc as i32;
                    let sweep = *sweep as i32;
                    let (x, y) = (end.x, end.y);
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
    let (input, (is_relative, args)) = svg_word(input, 'M', coordinate_pair_sequence)?;
    let word = SvgWordKind::MoveTo(args);
    Ok((input, SvgWord{ is_relative, word}))
}

fn line_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, (is_relative, args)) = svg_word(input, 'L', coordinate_pair_sequence)?;
    let word = SvgWordKind::LineTo(args);
    Ok((input, SvgWord{ is_relative, word}))
}

fn horizontal_line_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, (is_relative, args)) = svg_word(input, 'H', coordinate_sequence)?;
    let word = SvgWordKind::HorizontalLineTo(args);
    Ok((input, SvgWord{ is_relative, word}))
}

fn vertical_line_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, (is_relative, args)) = svg_word(input, 'V', coordinate_sequence)?;
    let word = SvgWordKind::VerticalLineTo(args);
    Ok((input, SvgWord{ is_relative, word}))
}

fn curve_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, (is_relative, args)) = svg_word(input, 'C', curve_to_coordinate_sequence)?;
    let word = SvgWordKind::CurveTo(args);
    Ok((input, SvgWord{ is_relative, word}))
}

fn curve_to_coordinate_sequence(input: &str) -> IResult<&str, CurveToCoordinateSequence> {
    many1(
        terminated(coordinate_pair_triplet, opt(comma_wsp))
    )(input)
}

fn smooth_curve_to(input: &str) -> IResult<&str, SvgWord> {
    let (input, (is_relative, args)) = svg_word(input, 'S', smooth_curve_to_coordinate_sequence)?;
    let word = SvgWordKind::SmoothCurveTo(args);
    Ok((input, SvgWord{ is_relative, word}))
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
    let (input, (is_relative, args)) = svg_word(input, 'Q', quadratic_bezier_curve_to_coordinate_sequence)?;
    let word = SvgWordKind::QuadraticBezierCurveTo(args);
    Ok((input, SvgWord{ is_relative, word}))
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
    let (input, (is_relative, args)) = svg_word(input, 'T', coordinate_pair_sequence)?;
    let word = SvgWordKind::SmoothQuadraticBezierCurveTo(args);
    Ok((input, SvgWord{ is_relative, word}))
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
    let (input, (is_relative, args)) = svg_word(input, 'A', elliptical_arc_argument_sequence)?;
    let word = SvgWordKind::EllipticalArc(args);
    Ok((input, SvgWord{ is_relative, word}))
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

fn close_path(input: &str) -> IResult<&str, SvgWord> {
    let (input, c) = satisfy(|c| c.to_ascii_uppercase() == 'Z')(input)?;
    let is_relative = c.is_ascii_lowercase();
    let word = SvgWordKind::ClosePath;
    Ok((input, SvgWord{ is_relative, word}))
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
    let (input, coord_pair) = separated_pair(
        coordinate,
        opt(comma_wsp),
        coordinate,
    )(input)?;
    Ok((input, coord_pair.into()))
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
    //let path = "M -23.69,-49.89 C -38.31,-49.89 -49.89,-37.14 -49.89,-21.57 C -49.89,10.32 -20.2,18.56 -0.1065,50 C 19.11,18.8 49.9,9.144 49.9,-21.57 C 49.9,-37.14 38.1,-49.89 23.69,-49.89 C 13.21,-49.89 4.044,-43.28 -0.1065,-33.6 C -4.256,-43.28 -13.21,-49.89 -23.69,-49.89 Z";
    let path = "M10,0 10,10L10,20 20,-15 -10, -10";
    println!("Original path:\n\t{path}");


    let (_remaining, mut path) = svg_path(path)?;
    let mut path = SvgPathStruct(&mut path);
    path.scale(2.0);

    println!("As new path:\n\t{}", path);

    let bb = path.bounding_box((0.0, 0.0).into());
    println!("bb:\n\t{:?}", bb);

    Ok(())

}
