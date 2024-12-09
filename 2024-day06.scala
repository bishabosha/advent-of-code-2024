package day06

import challenges.*
import stringmatching.regex.Interpolators.r
import scala.util.boundary, boundary.break


// import Domain.Letter, Letter.*

def part1(input: String): Int =
  val in = parse(input)
  // TODO: organize obstacles via row and column (two lookups)
  // depending in direction, we can move in a certain direction
  val byCol = in.obstacles.groupBy(_.x).view.mapValues(_.sortBy(_.y)).toMap
  val byRow = in.obstacles.groupBy(_.y).view.mapValues(_.sortBy(_.x)).toMap
  var dir = 0
  var pos = in.guard
  while true do
    val cDir = dirs(dir)
    if cDir.dy != 0 then
      val row = byRow(pos.y)
      if cDir.dy == -1 then
        val inFront = row.reverse.dropWhile(_.y > pos.y).headOption.map(p => (x = p.x, y = p.y + 1))
        // if inFront is not empty, push a path segment and rotate, else break
        ???
      else
        val inFront = row.dropWhile(_.y < pos.y).headOption.map(p => (x = p.x, y = p.y - 1))
        // if inFront is not empty, push a path segment and rotate, else break
        ???
    else
      val col = byCol(pos.x)
      if cDir.dx == -1 then
        val inFront = col.reverse.dropWhile(_.x > pos.x).headOption.map(p => (x = p.x + 1, y = p.y))
        // if inFront is not empty, push a path segment and rotate, else break
        ???
      else
        val inFront = col.dropWhile(_.x < pos.x).headOption.map(p => (x = p.x - 1, y = p.y))
        // if inFront is not empty, push a path segment and rotate, else break
        ???
  end while
  // TODO: calculate total path length, in terms of unique squares visited.
  -1


def part2(input: String): Int =
  -1

val dirs = IArray(
  (dy = -1, dx = 0), // up
  (dy = 0, dx = 1), // right
  (dy = 1, dx = 0), // down
  (dy = 0, dx = -1) // left
)

def rotate(dir: Int) = (dir + 1) % 4

// object Domain:
//   opaque type Letter = Byte

//   object Letter:
//     val Obstacle: Letter = (1 << 0).toByte
//     val Space: Letter = (1 << 1).toByte
//     val Guard: Letter = (1 << 2).toByte

//     def unsafeFromChar(char: Char): Letter = char match
//       case '#' => Obstacle
//       case '.' => Space
//       case '^' => Guard

type Point = (x: Int, y: Int)

def parse(input: String) =
  val buf = List.newBuilder[Point]
  var guard: Point | Null = null
  var dims: Point | Null = null
  def loop(y: Int, x: Int, i: Int): Unit =
    if i == input.length then ()
    else input(i) match
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
