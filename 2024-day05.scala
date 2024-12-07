package day05

import challenges.*
import stringmatching.regex.Interpolators.r
import scala.util.boundary, boundary.break

def part1(input: String): Int =
  val in = parse(input)
  in.updates.view
    .filter(correct(_, in.lookup))
    .map(update => update(update.size/2))
    .sum

def parse(input: String) =
  val r"$rules\n\n$updates" = input.runtimeChecked
  val r"${r"$xs%d|$ys%d"}...(\n)" = rules.runtimeChecked
  val r"${r"${r"$iss%d"}...(,)"}...(\n)" = updates.runtimeChecked
  val lookup = xs.lazyZip(ys).groupMapReduce((x, _) => x)((_, y) => Set(y))(_ ++ _)
  (lookup = lookup.withDefaultValue(Set.empty), updates = iss)

def correct(update: Seq[Int], lookup: Map[Int, Set[Int]]): Boolean =
  var seen = Set.empty[Int]
  boundary:
    for i <- update do
      if seen.exists(lookup(i)) then
        break(false)
      seen = seen + i
    true

def part2(input: String): Int =
  val in = parse(input)
  in.updates.view
    .filterNot(correct(_, in.lookup))
    .map: update =>
      update
        .sortWith((x, y) => in.lookup(x).contains(y))
        .apply(update.size/2)
    .sum


@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
