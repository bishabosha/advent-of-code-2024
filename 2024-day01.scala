package day01

import challenges.*
import stringmatching.regex.Interpolators.r

def part1(input: String): Int =
  val (is, js) = lists(input)
  is.sorted
    .lazyZip(js.sorted)
    .map: (l, r) =>
      if r < l then l - r else r - l
    .sum

def part2(input: String): Int =
  val (is, js) = lists(input)

  val right = js.groupMapReduce(identity)(_ => 1)(_ + _)
  is.map(k => k * right.getOrElse(k, 0)).sum

def lists(input: String) =
  val r"${r"$is%d   $js%d"}...(\n)" = input.runtimeChecked
  (left = is, right = js)

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
