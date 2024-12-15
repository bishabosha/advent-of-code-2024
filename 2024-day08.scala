package day08

import challenges.*
import stringmatching.regex.Interpolators.r
import scala.util.boundary, boundary.break

def part1(input: String): Long =
  val in = parse(input)
  solve(in): (p1, p2) =>
    val dy = p2.y - p1.y
    val dx = p2.x - p1.x
    val a1 = (p1.x - dx, p1.y - dy)
    val a2 = (p2.x + dx, p2.y + dy)
    Iterator(a1, a2).filter(inBounds(in, _))

def part2(input: String): Long =
  val in = parse(input)
  solve(in): (p1, p2) =>
    val dy = p2.y - p1.y
    val dx = p2.x - p1.x
    val befores = Iterator.unfold(p1): p0 =>
      val next = (x = p0.x - dx, y = p0.y - dy)
      Option.when(inBounds(in, next))(next -> next)
    val afters = Iterator.unfold(p2): p0 =>
      val next = (x = p0.x + dx, y = p0.y + dy)
      Option.when(inBounds(in, next))(next -> next)
    befores ++ afters ++ Iterator(p1, p2)

def solve(in: Input)(fillNodes: (Point, Point) => Iterator[Point]): Int =
  val byFreq = in.antennae.groupMap(_.freq)(_.point).values
  val antinodes = byFreq.foldLeft(Set.empty[Point]): (acc, points) =>
    points.combinations(2).foldLeft(acc): (acc, pair) =>
      val p1 :: p2 :: Nil = pair.runtimeChecked
      acc ++ fillNodes(p1, p2)
  antinodes.size

def inBounds(input: Input, point: Point): Boolean =
  val (x = x, y = y) = point
  val (width = w, height = h) = input.size
  x >= 0 && x < w && y >= 0 && y < h

type Point = (x: Int, y: Int)
type Size = (width: Int, height: Int)
type Antenna = (freq: Char, point: Point)
type Input = (antennae: List[Antenna], size: Size)
def parse(input: String): Input =
  val buf = List.newBuilder[Antenna]
  var size: Size | Null = null
  def loop(y: Int, x: Int, i: Int): Unit =
    if i == input.length then ()
    else
      input(i) match
        case '\n' =>
          size = (width = x, height = y + 1)
          loop(y + 1, 0, i + 1)
        case '.' =>
          loop(y, x + 1, i + 1)
        case c =>
          buf += ((freq = c, point = (x = x, y = y)))
          loop(y, x + 1, i + 1)
  loop(0, 0, 0)
  (antennae = buf.result(), size = size.nn)

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
