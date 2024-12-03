package day03

import challenges.*
import stringmatching.regex.Interpolators.r
import scala.util.boundary, boundary.break

def part1(input: String): Int =
  parse(input, Mode.Skip)

def part2(input: String): Int =
  parse(input, Mode.Do)

enum State:
  case None, Mul, Arg1, Arg2, Do, Dont

enum Mode:
  case Do, Dont, Skip

def parse(input: String, mode: Mode) =
  def loop(
      i: Int,
      prev: Char,
      arg1: Int,
      arg2: Int,
      state: State,
      mode: Mode,
      sum: Int
  ): Int =
    if i == input.length then sum
    else
      input(i) match
        case 'd' if mode != Mode.Skip && state == State.None =>
          loop(i + 1, 'd', 0, 0, State.Do, mode, sum)
        case 'o' if mode != Mode.Skip && state == State.Do && prev == 'd' =>
          loop(i + 1, 'o', 0, 0, State.Do, mode, sum)
        case '('
            if mode != Mode.Skip && (state == State.Do && prev == 'o' || state == State.Dont && prev == 't') =>
          loop(i + 1, '(', 0, 0, state, mode, sum)
        case ')'
            if mode != Mode.Skip && (state == State.Do || state == State.Dont) && prev == '(' =>
          loop(
            i + 1,
            ')',
            0,
            0,
            State.None,
            if state == State.Do then Mode.Do else Mode.Dont,
            sum
          )
        case 'n' if mode != Mode.Skip && state == State.Do && prev == 'o' =>
          loop(i + 1, 'n', 0, 0, State.Dont, mode, sum)
        case '\'' if mode != Mode.Skip && state == State.Dont && prev == 'n' =>
          loop(i + 1, '\'', 0, 0, State.Dont, mode, sum)
        case 't' if mode != Mode.Skip && state == State.Dont && prev == '\'' =>
          loop(i + 1, 't', 0, 0, State.Dont, mode, sum)
        case 'm' if state == State.None && mode != Mode.Dont =>
          loop(i + 1, 'm', 0, 0, State.Mul, mode, sum)
        case 'u' if state == State.Mul && prev == 'm' && mode != Mode.Dont =>
          loop(i + 1, 'u', 0, 0, State.Mul, mode, sum)
        case 'l' if state == State.Mul && prev == 'u' && mode != Mode.Dont =>
          loop(i + 1, 'l', 0, 0, State.Mul, mode, sum)
        case '(' if state == State.Mul && prev == 'l' && mode != Mode.Dont =>
          loop(i + 1, '(', 0, 0, State.Arg1, mode, sum)
        case n if n.isDigit && state == State.Arg1 && mode != Mode.Dont =>
          loop(i + 1, n, arg1 * 10 + n.asDigit, 0, State.Arg1, mode, sum)
        case ',' if state == State.Arg1 && prev != '(' && mode != Mode.Dont =>
          loop(i + 1, ',', arg1, 0, State.Arg2, mode, sum)
        case n if n.isDigit && state == State.Arg2 && mode != Mode.Dont =>
          loop(i + 1, n, arg1, arg2 * 10 + n.asDigit, State.Arg2, mode, sum)
        case ')' if state == State.Arg2 && prev != ',' && mode != Mode.Dont =>
          loop(i + 1, ')', 0, 0, State.None, mode, sum + (arg1 * arg2))
        case c => loop(i + 1, c, 0, 0, State.None, mode, sum)
  end loop
  loop(0, '\u0000', 0, 0, State.None, mode, 0)

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
