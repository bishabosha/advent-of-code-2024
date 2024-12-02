package day02

import challenges.*
import stringmatching.regex.Interpolators.r
import scala.util.boundary, boundary.break

def part1(input: String): Int =
  parse(input).count(isSafe)

def part2(input: String): Int =
  parse(input).count(isTolerantSafe)

def parse(input: String) =
  val r"${r"${r"$reports%d"}...( )"}...(\n)" = input.runtimeChecked
  reports

def isSafe(report: IndexedSeq[Int]): Boolean = boundary:
  var seen = false
  var sign = 0
  for case Seq(i, j) <- report.sliding(2) do
    val report0 = report
    val sign0 = (j - i).sign
    val diff = (j - i).abs
    if diff < 1 || diff > 3 then
      break(false)
    if !seen then
      seen = true
      if sign0 == 0 then
        break(false)
      sign = sign0
    else
      if sign != sign0 then
        break(false)
  true

def isTolerantSafe(report: IndexedSeq[Int]): Boolean =
  isSafe(report) || report.indices.exists { i =>
    val (init, rest) = report.splitAt(i)
    isSafe(init ++ rest.tail)
  }

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
