package day04

import challenges.*
import stringmatching.regex.Interpolators.r
import scala.util.boundary, boundary.break

import Domain.Letter, Letter.*

def part1(input: String): Int =
  solve(input, countXMAS)

def part2(input: String): Int =
  solve(input, countMAS)

type Grid = IArray[IArray[Letter]]

def solve(input: String, f: (Int, Int, Grid) => Int): Int =
  val grid: Grid = parse(input)
  var total = 0
  for
    x <- grid.indices
    y <- grid(x).indices
  do total += f(x, y, grid)
  total

val dirs = IArray(
  (dx = 0, dy = 1), // up
  (dx = 1, dy = 0), // right
  (dx = 0, dy = -1), // down
  (dx = -1, dy = 0), // left
  (dx = 1, dy = 1), // up-right
  (dx = 1, dy = -1), // down-right
  (dx = -1, dy = -1), // down-left
  (dx = -1, dy = 1) // up-left
)

val UpRight = dirs(4)
val DownRight = dirs(5)
val DownLeft = dirs(6)
val UpLeft = dirs(7)

val dirsMAS = IArray(
  UpRight -> DownLeft,
  DownRight -> UpLeft,
  DownLeft -> UpRight,
  UpLeft -> DownRight
)

def countXMAS(x: Int, y: Int, grid: Grid): Int =
  dirs.count(dir => scan(x, y, dir.dy, dir.dx, Zero, grid))

def countMAS(x: Int, y: Int, grid: Grid): Int =
  grid(x)(y).match
    case A =>
      val seen = dirsMAS.count: (tranform, dir) =>
        scanMAS(x + tranform.dx, y + tranform.dy, dir.dy, dir.dx, Zero, grid)
      seen / 2
    case _ => 0

def boundCheck(x: Int, y: Int, grid: Grid): Boolean =
  x >= 0 && x < grid.length && y >= 0 && y < grid(0).length

def scan(x: Int, y: Int, dy: Int, dx: Int, prev: Letter, grid: Grid): Boolean =
  boundCheck(x, y, grid) && grid(x)(y).match
    case X if prev == Zero => scan(x + dx, y + dy, dy, dx, X, grid)
    case M if prev == X    => scan(x + dx, y + dy, dy, dx, M, grid)
    case A if prev == M    => scan(x + dx, y + dy, dy, dx, A, grid)
    case S if prev == A    => true
    case _                 => false

def scanMAS(x: Int, y: Int, dy: Int, dx: Int, prev: Letter, grid: Grid): Boolean =
  boundCheck(x, y, grid) && grid(x)(y).match
    case M if prev == Zero => scan(x + dx, y + dy, dy, dx, M, grid)
    case A if prev == M    => scan(x + dx, y + dy, dy, dx, A, grid)
    case S if prev == A    => true
    case _                 => false

object Domain:
  opaque type Letter = Byte

  object Letter:
    val X: Letter = (1 << 0).toByte
    val M: Letter = (1 << 1).toByte
    val A: Letter = (1 << 2).toByte
    val S: Letter = (1 << 3).toByte
    val Zero: Letter = 0.toByte

    def unsafeFromChar(char: Char): Letter = char match
      case 'X' => X
      case 'M' => M
      case 'A' => A
      case 'S' => S

def parse(input: String): Grid =
  IArray.from(
    input.linesIterator.map(s => IArray.from(s.iterator.map(Letter.unsafeFromChar)))
  )

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
