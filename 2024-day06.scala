package day06

import challenges.*
import stringmatching.regex.Interpolators.r
import scala.util.boundary, boundary.break
import scala.collection.View
import scala.collection.BitSet

def part1(input: String): Int =
  fillAll(parse(input)).size

def part2(input: String): Int =
  val in = parse(input)
  val candidates = fillAll(in) - in.guard
  val points = candidates.count: candidate =>
    val in0 = (guard = in.guard, obstacles = candidate :: in.obstacles, dims = in.dims)
    boundary:
      go(in0)(Map.empty[Point, BitSet]): (visited, pos, p, dir) =>
        fill(pos, p, confs(dir).dir).foldLeft(visited): (visited0, p0) =>
          visited0.updatedWith(p0):
            case Some(seen) =>
              if seen(dir) then break(true)
              else Some(seen + dir)
            case None =>
              Some(BitSet.empty + dir)
      false
  points

def fillAll(in: Input): Set[Point] =
  go(in)(Set.newBuilder[Point])
    .apply: (acc, pos, p, dir) =>
      acc ++= fill(pos, p, confs(dir).dir)
    .result()

def fill(point: Point, p2: Point, cDir: Dir): Iterator[Point] =
  Iterator.single(point) ++
    Iterator
      .unfold(point): p =>
        if p == p2 then None
        else
          val next = (x = p.x + cDir.dx, y = p.y + cDir.dy)
          Some(next -> next)

def go[Z](in: Input)(z: Z)(f: (Z, Point, Point, Int) => Z): Z =
  val byCol = in.obstacles
    .groupBy(_.x)
    .view
    .mapValues(row =>
      val forward = row.sortBy(_.y)
      (forward = forward.view, backward = forward.reverse.view)
    )
    .toMap
  val byRow = in.obstacles
    .groupBy(_.y)
    .view
    .mapValues(col =>
      val forward = col.sortBy(_.x)
      (forward = forward.view, backward = forward.reverse.view)
    )
    .toMap
  val lookup = (rows = byRow, cols = byCol)
  var dir = 0
  var pos = in.guard
  var acc = z
  boundary:
    while true do
      val conf = confs(dir)
      val cDir = conf.dir
      conf.look(lookup, pos) match
        case None =>
          val bound = conf.bound(in.dims, pos)
          break(f(acc, pos, bound, dir))
        case Some(p) =>
          val oppositeDir = confs(opposite(dir)).dir
          val p0 = (x = p.x + oppositeDir.dx, y = p.y + oppositeDir.dy)
          acc = f(acc, pos, p0, dir)
          pos = p0
          dir = rotate(dir)
    end while
    acc
end go

type RowCol = (forward: View[Point], backward: View[Point])
type Lookup = (rows: Map[Int, RowCol], cols: Map[Int, RowCol])
type Dir = (dy: Int, dx: Int)

def config(dir: Dir, look: (Lookup, Point) => Option[Point], bound: (Point, Point) => Point) =
  (dir = dir, look = look, bound = bound)

val confs = IArray(
  config(
    (dy = -1, dx = 0), // up
    (lookup, pos) => lookup.cols.get(pos.x).flatMap(_.backward.dropWhile(_.y > pos.y).headOption),
    (dims, pos) => (x = pos.x, y = 0)
  ),
  config(
    (dy = 0, dx = 1), // right
    (lookup, pos) => lookup.rows.get(pos.y).flatMap(_.forward.dropWhile(_.x < pos.x).headOption),
    (dims, pos) => (x = dims.x, y = pos.y)
  ),
  config(
    (dy = 1, dx = 0), // down
    (lookup, pos) => lookup.cols.get(pos.x).flatMap(_.forward.dropWhile(_.y < pos.y).headOption),
    (dims, pos) => (x = pos.x, y = dims.y)
  ),
  config(
    (dy = 0, dx = -1), // left
    (lookup, pos) => lookup.rows.get(pos.y).flatMap(_.backward.dropWhile(_.x > pos.x).headOption),
    (dims, pos) => (x = 0, y = pos.y)
  )
)

def rotate(dir: Int) = (dir + 1) % 4
def opposite(dir: Int) = rotate(rotate(dir))

type Point = (x: Int, y: Int)
type Input = (guard: Point, obstacles: List[Point], dims: Point)

def parse(input: String): Input =
  val buf = List.newBuilder[Point]
  var guard: Point | Null = null
  var dims: Point | Null = null
  def loop(y: Int, x: Int, i: Int): Unit =
    if i == input.length then ()
    else
      input(i) match
        case '\n' =>
          dims = (x = x - 1, y = y)
          loop(y + 1, 0, i + 1)
        case '#' =>
          buf += ((x, y))
          loop(y, x + 1, i + 1)
        case '.' =>
          loop(y, x + 1, i + 1)
        case '^' =>
          guard = (x, y)
          loop(y, x + 1, i + 1)
  loop(0, 0, 0)
  (guard = guard.nn, obstacles = buf.result(), dims = dims.nn)

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
