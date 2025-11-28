signature DAY_IMPL =
sig
  type t
  type result1
  type result2

  val id: string
  val parse: string list -> t

  val solve1: t -> result1
  val solve2: t -> result2

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
    { id = id
    , solve1 = fn input => display1 (solve1 (parse input))
    , solve2 = fn input => display2 (solve2 (parse input))
    }
end
