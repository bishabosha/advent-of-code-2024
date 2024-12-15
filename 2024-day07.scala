package day07

import challenges.*
import stringmatching.regex.Interpolators.r
import scala.util.boundary, boundary.break

def part1(input: String): Long =
  parse(input).filter(p =>
    val acc +: rest = p.inputs.runtimeChecked
    choice(p.value, Seq(_ + _, _ * _), acc, rest)
  ).map(_.value).sum

def choice(test: Long, ops: Seq[(Long, Long) => Long], acc: Long, in: Seq[Long]): Boolean =
  in match
    case Nil => acc == test
    case x +: xs => ops.exists(op => choice(test, ops, op(acc, x), xs))

def part2(input: String): Long =
  parse(input).filter(p =>
    val acc +: rest = p.inputs.runtimeChecked
    choice(p.value, Seq(_ + _, _ * _, (l, r) => mergeDigits(splitDigits(l), splitDigits(r))), acc, rest)
  ).map(_.value).sum

def mergeDigits(left: IArray[Byte], right: IArray[Byte]): Long =
  def mergeDigit(acc: Long, digit: Byte): Long =
    acc * 10 + digit
  right.foldLeft(left.foldLeft(0L)(mergeDigit))(mergeDigit)

def splitDigits(n: Long): IArray[Byte] =
  var n0 = n
  var size = 0
  while n0 > 0 do
    n0 /= 10
    size += 1
  val digits = new Array[Byte](size)
  n0 = n
  while n0 > 0 do
    digits(size - 1) = (n0 % 10).toByte
    n0 /= 10
    size -= 1
  IArray.unsafeFromArray(digits)

def parse(input: String): Seq[(value: Long, inputs: Seq[Long])] =
  val r"${r"$xs%L: ${r"$yss%L"}...( )"}...(\n)" = input.runtimeChecked
  xs.zip(yss)

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
