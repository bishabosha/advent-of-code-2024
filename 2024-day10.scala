package day10

import challenges.*
import stringmatching.regex.Interpolators.r
import scala.util.boundary, boundary.break
import scala.collection.mutable.Builder
import scala.annotation.tailrec
import scala.collection.immutable.BitSet

def part1(input: String): Int =
  solve(input)((_, id) => id)

def part2(input: String): Int =
  solve(input)((s, id) => (s + id).hashCode)

val dirs = IArray(
  (dy = -1, dx = 0), // up
  (dy = 0, dx = 1), // right
  (dy = 1, dx = 0), // down
  (dy = 0, dx = -1) // left
)

def translate(x: Int, y: Int, dir: Int): (x: Int, y: Int) =
  val cDir = dirs(dir)
  (x + cDir.dx, y + cDir.dy)

def inBounds(x: Int, y: Int, grid: IArray[IArray[Byte]]): Boolean =
  x >= 0 && x < grid(0).size && y >= 0 && y < grid.size

def find(acc: Map[Int, Set[Int]], explore: List[Path], grid: IArray[IArray[Byte]])(f: (BitSet, Int) => Int): Map[Int, Set[Int]] =
  explore.runtimeChecked match
    case Nil => acc
    case (seen = s, x = x, y = y, origin = o) :: rest =>
      val current = grid(y)(x)
      val (endings, paths0) = dirs.indices.map(translate(x, y, _)).collect {
        case (x = x0, y = y0) if inBounds(x0, y0, grid) && grid(y0)(x0) == current + 1 && !s(id(x0, y0, grid)) =>
          val id0 = id(x0, y0, grid)
          val height = grid(y0)(x0)
          if height == 9 then Left(f(s, id0))
          else
            Right((seen = s + id0, x = x0, y = y0, origin = o))
      }.partitionMap(identity)
      find(
        acc.updatedWith(o)(z => Some(z.fold(Set(endings*))(_ ++ endings))),
        paths0 ++: rest,
        grid
      )(f)


def solve(input: String)(f: (BitSet, Int) => Int): Int =
  val grid = parse(input)
  val origins = Iterator
    .tabulate(grid.size, grid.size): (y, x) =>
      if grid(y)(x) == 0 then
        val id0 = id(x, y, grid)
        Some((seen = BitSet.empty + id0, x = x, y = y, origin = id0)) else None
    .flatten
    .flatten
    .toList
  val found = find(Map.empty, origins, grid)(f)
  found.values.map(_.size).sum

def id(x: Int, y: Int, grid: IArray[IArray[Byte]]): Int = y * grid(0).size + x

def parse(input: String): IArray[IArray[Byte]] =
  IArray.from(input.linesIterator.map(line => IArray.from(line.map(_.asDigit.toByte))))

type Path = (seen: BitSet, x: Int, y: Int, origin: Int)

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
