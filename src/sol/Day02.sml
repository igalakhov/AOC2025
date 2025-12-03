structure Day02Impl: DAY_IMPL =
struct
  val day = 2

  type state = (IntInf.int * IntInf.int) list

  type result1 = IntInf.int
  type result2 = IntInf.int

  val stoi = Option.valOf o IntInf.fromString

  fun parse [line] =
        let
          fun parseOne [x, y] = (stoi x, stoi y)
            | parseOne _ = raise ParseError
        in
          List.map (parseOne o (String.tokens (fn x => x = #"-")))
            (String.tokens (fn x => x = #",") line)
        end
    | parse _ = raise ParseError

  fun lessThanWithRepeat limit repeat =
    let
      fun get (cur, acc) =
        let
          val id = (stoi o String.concat) (List.tabulate (repeat, fn _ =>
            IntInf.toString cur))
        in
          if id > limit then acc else get (cur + (IntInf.fromInt 1), id :: acc)
        end
    in
      get (IntInf.fromInt 1, [])
    end

  fun lessThan limit =
    let
      fun get repeat =
        let val next = lessThanWithRepeat limit repeat
        in if List.null next then [] else next :: (get (repeat + 1))
        end
    in
      (Util.dedup o Util.sort IntInf.compare o List.concat) (get 2)
    end

  val solve1 =
    (List.foldl op+ 0) o List.concat
    o
    (List.map (fn (lo, hi) =>
       (List.filter (fn id => id >= lo) (lessThanWithRepeat hi 2))))

  val solve2 =
    (List.foldl op+ 0) o Util.sort IntInf.compare o List.concat
    o
    (List.map (fn (lo, hi) => (List.filter (fn id => id >= lo) (lessThan hi))))

  val display1 = IntInf.toString
  val display2 = IntInf.toString
end

structure Day02: DAY = MakeDay(Day02Impl)
