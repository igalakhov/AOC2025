structure Day03Impl: DAY_IMPL =
struct
  val day = 3

  type state = int list list

  type result1 = IntInf.int
  type result2 = IntInf.int

  val parse = List.map (List.map (Util.stoi o String.str) o String.explode)

  fun maxNum numDigits nums =
    let
      val mem: IntInf.int option Array2.array =
        Array2.array (numDigits + 1, Array.length nums, NONE)
      fun go nd idx =
        if idx + nd > Array.length nums then
          0
        else if nd = 1 then
          Array.foldli
            (fn (idx', elem, acc) =>
               if idx' >= idx then IntInf.max (elem, acc) else acc) 0 nums
        else
          case Array2.sub (mem, nd, idx) of
            SOME ans => ans
          | NONE =>
              (let
                 val d1 = Array.sub (nums, idx)
                 val ans =
                   IntInf.max
                     ( go nd (idx + 1)
                     , (Option.valOf o IntInf.fromString)
                         (IntInf.toString d1
                          ^ (IntInf.toString (go (nd - 1) (idx + 1))))
                     )
                 val () = Array2.update (mem, nd, idx, SOME ans)
               in
                 ans
               end)
    in
      go numDigits 0
    end

  fun solveWithNumDigits numDigits =
    (List.foldl op+ 0) o List.map (maxNum numDigits)
    o List.map (Array.fromList o List.map IntInf.fromInt)

  val solve1 = solveWithNumDigits 2
  val solve2 = solveWithNumDigits 12

  val display1 = IntInf.toString
  val display2 = IntInf.toString
end

structure Day03: DAY = MakeDay(Day03Impl)
