package day09

import challenges.*
import stringmatching.regex.Interpolators.r
import scala.util.boundary, boundary.break
import scala.collection.mutable.Builder
import scala.annotation.tailrec
import scala.collection.SortedMap

def part1(input: String): Long =
  val map = parse(input)
  val compacted = compact(i = 0, j = map.size - 1, shift = false, map, map.last, IArray.newBuilder)
  compacted.zipWithIndex.map { (v, i) => v.toLong * i }.sum

def parse(input: String): IArray[Byte] =
  IArray.from(input.linesIterator.next.map(_.asDigit.toByte))

def debug(i: Int, shift: Boolean, map: IArray[Byte], acc: Builder[String, IArray[String]]): IArray[String] =
  if i == map.size then acc.result()
  else
    val idL = i / 2
    val count = map(i)
    val key = if shift then "." else idL.toString
    for _ <- 0 until count do acc += key
    debug(i + 1, !shift, map, acc)

@tailrec
def compact(i: Int, j: Int, shift: Boolean, map: IArray[Byte], rem: Int, acc: Builder[Int, IArray[Int]]): IArray[Int] =
  if i == map.size then acc.result()
  else
    val idL = i / 2
    val idR = j / 2
    if !shift then
      val (terminal, count) = if idL == idR then (true, rem) else (false, map(i).toInt)
      for _ <- 0 until count do acc += idL
      if terminal then acc.result()
      else compact(i + 1, j, !shift, map, rem, acc)
    else
      val count = map(i) // spaces to fill
      var rem0 = rem
      var idR0 = idR
      var j0 = j
      val terminal = boundary:
        for _ <- 0 until count do
          if rem0 == 0 then
            if idR0 == idL + 1 then break(true)
            j0 = j0 - 2
            idR0 = idR0 - 1
            rem0 = map(j0)
          end if
          acc += idR0
          rem0 -= 1
        end for
        false
      if terminal then acc.result()
      else compact(i + 1, j0, !shift, map, rem0, acc)

def part2(input: String): Long =
  val map = parse(input)
  def sp = spaces(i = 0, shift = false, map = map, acc = SortedMap.empty)
  val (seen, filled) = compact2(j = map.size - 1, shift = false, map, sp, Set.empty, Map.empty)
  val compacted = compose(i = 0, shift = false, map, seen, filled, IArray.newBuilder)
  compacted.view.zipWithIndex.collect { case (v, i) if v > 0 => v.toLong * i }.sum

@tailrec
def compact2(
    j: Int,
    shift: Boolean,
    map: IArray[Byte],
    sp: SortedMap[Int, Int],
    moved: Set[Int],
    acc: Map[Int, IArray[Int]]
): (Set[Int], Map[Int, IArray[Int]]) =
  if j < 0 then (moved, acc)
  else
    val count = map(j)
    val idR = j / 2
    sp.find((k, v) => k < j && v >= count) match
      case Some((k, v)) =>
        val sp2 = sp + (k -> (v - count))
        val acc2 = acc.updatedWith(k):
          case None    => Some(IArray.fill(count)(idR))
          case Some(v) => Some(v ++ Iterator.fill(count)(idR))
        val moved2 = moved + j
        compact2(j = j - 2, shift = !shift, map, sp2, moved2, acc2)
      case None =>
        compact2(j = j - 2, shift = !shift, map, sp, moved, acc)

@tailrec
def compose(
    i: Int,
    shift: Boolean,
    map: IArray[Byte],
    moved: Set[Int],
    spaces: Map[Int, IArray[Int]],
    acc: Builder[Int, IArray[Int]]
): IArray[Int] =
  if i == map.size then acc.result()
  else
    val count = map(i)
    if !shift then
      val idL = i / 2
      val key = if moved(i) then -1 else idL
      for _ <- 0 until count do acc += key
      compose(i + 1, !shift, map, moved, spaces, acc)
    else
      val filled = spaces.getOrElse(i, IArray.empty[Int])
      for idR <- filled do acc += idR
      for _ <- 0 until count - filled.size do acc += -1
      compose(i + 1, !shift, map, moved, spaces, acc)

def spaces(i: Int, shift: Boolean, map: IArray[Byte], acc: SortedMap[Int, Int]): SortedMap[Int, Int] =
  if i == map.size then acc
  else if !shift then spaces(i + 1, !shift, map, acc)
  else spaces(i + 1, !shift, map, acc + (i -> map(i)))

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday(""))}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday(""))}")
