structure Day01Impl: DAY_IMPL =
struct
  type t = string list

  type result1 = int
  type result2 = int

  val id = "sample_day_01"

  fun parse input = input

  fun solve1 _ = 1
  fun solve2 _ = 2

  val display1 = Int.toString
  val display2 = Int.toString
end

structure Day01: DAY = MakeDay(Day01Impl)
