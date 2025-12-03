structure Day01Impl: DAY_IMPL =
struct
  val day = 1

  datatype move = R of int | L of int

  type state = move list

  type result1 = int
  type result2 = int

  val parse =
    let
      val parseChars = Util.stoi o String.implode

      fun parseOne (#"L" :: cs) =
            L (parseChars cs)
        | parseOne (#"R" :: cs) =
            R (parseChars cs)
        | parseOne _ = raise ParseError
    in
      List.map (parseOne o String.explode)
    end

  val getVals =
    (fn (_, ps) => List.rev ps)
    o
    (List.foldl
       (fn (move, (p, ps)) =>
          let
            val p' =
              (case move of
                 L amt => p - amt
               | R amt => p + amt) mod 100
          in
            (p', p' :: ps)
          end) (50, [50]))


  val solve1 = (Util.count (fn x => x = 0)) o (getVals)

  val solve2 =
    solve1 o List.concat
    o
    (List.map (fn move =>
       case move of
         L amt => List.tabulate (amt, (fn _ => L 1))
       | R amt => List.tabulate (amt, (fn _ => R 1))))

  val display1 = Int.toString
  val display2 = Int.toString
end

structure Day01: DAY = MakeDay(Day01Impl)
