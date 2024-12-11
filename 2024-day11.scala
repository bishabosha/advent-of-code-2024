package day11

import challenges.*
import stringmatching.regex.Interpolators.r
import scala.util.boundary, boundary.break

def part1(input: String): Int =
  val stones = parse(input)
  Iterator
    .iterate(stones)(stepOrdered)
    .drop(25)
    .next
    .size

def mergeDigits(digits: Array[Long]): Long =
  digits.iterator
    .dropWhile(_ == 0)
    .foldLeft(0L): (acc, digit) =>
      acc * 10 + digit

def splitDigits(n: Long): Option[(Long, Long)] =
  val digits = Iterator
    .unfold(n):
      case 0 => None
      case i => Some((i % 10, i / 10))
    .toArray
  if digits.size % 2 == 0 then
    val (a, b) = digits.reverse.splitAt(digits.size / 2)
    Some((mergeDigits(a), mergeDigits(b)))
  else None

object EvenDigits:
  def unapply(n: Long): Option[(Long, Long)] =
    splitDigits(n)

def stepOrdered(stones: Seq[Long]): Seq[Long] =
  stones.flatMap:
    case 0                => 1 :: Nil
    case EvenDigits(a, b) => a :: b :: Nil
    case other            => other * 2024 :: Nil

def step(stones: Map[Long, Long]): Map[Long, Long] =
  stones.foldLeft(stones): (stones, stone) =>
    stone match
      case (0L, count)                     => stones.pop(0, count).push(1, count)
      case (old @ EvenDigits(a, b), count) => stones.pop(old, count).push(a, count).push(b, count)
      case (other, count)                  => stones.pop(other, count).push(other * 2024, count)

extension (stones: Map[Long, Long])
  def push(stone: Long, count: Long): Map[Long, Long] =
    stones.updatedWith(stone):
      case None    => Some(count)
      case Some(n) => Some(n + count)

  def pop(stone: Long, count: Long): Map[Long, Long] =
    stones.updatedWith(stone):
      case None    => ???
      case Some(n) => Some(n - count)

  def totalStones: Long =
    stones.values.sum

def part2(input: String): Long =
  val stones = parse(input).map(_ -> 1L).toMap
  Iterator
    .iterate(stones)(step)
    .drop(75)
    .next
    .totalStones

def parse(input: String) =
  val r"${r"$is%L"}...( )" = input.runtimeChecked
  is

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
