exception ParseError

signature DAY_IMPL =
sig
  val day: int

  type state
  type result1
  type result2

  val parse: string list -> state

  val solve1: state -> result1
  val solve2: state -> result2

  val display1: result1 -> string
  val display2: result2 -> string
end

type DaySolution =
  {id: string, solve1: string list -> string, solve2: string list -> string}

signature DAY =
sig
  val solution: DaySolution
end

functor MakeDay(Impl: DAY_IMPL): DAY =
struct
  open Impl

  val solution =
    { id = Int.toString day
    , solve1 = fn input => display1 (solve1 (parse input))
    , solve2 = fn input => display2 (solve2 (parse input))
    }
end
