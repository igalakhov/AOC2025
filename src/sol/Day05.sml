structure Day05Impl: DAY_IMPL =
struct
  val day = 5

  type state = (IntInf.int * IntInf.int) list * IntInf.int list

  type result1 = int
  type result2 = IntInf.int

  val stoi = Option.valOf o IntInf.fromString

  fun parse lines =
    let
      val (ranges, nums) =
        ((List.partition (String.isSubstring "-"))
         o (List.filter (not o List.null o String.explode))) lines
      fun parseRange range =
        (case String.tokens (fn c => c = #"-") range of
           [l, r] => (stoi l, stoi r)
         | _ => raise ParseError)
    in
      (List.map parseRange ranges, List.map stoi nums)
    end

  fun solve1 (ranges, nums) =
    List.length
      (List.filter
         (fn num =>
            (not o List.null)
              (List.filter
                 (fn (l, r) =>
                    (op IntInf.<=) (l, num) andalso (op IntInf.<=) (num, r))
                 ranges)) nums)

  fun solve2 (ranges, _) =
    let
      fun merge [] = []
        | merge [r] = [r]
        | merge ((l1, r1) :: (l2, r2) :: rs) =
            if (op IntInf.<) (r1, l2) then (l1, r1) :: merge ((l2, r2) :: rs)
            else merge ((l1, IntInf.max (r1, r2)) :: rs)

      val sortedRanges =
        Util.sort (fn ((l1, _), (l2, _)) => IntInf.compare (l1, l2)) ranges
    in
      ((List.foldl (op IntInf.+) 0) o List.map (fn (l, r) => r - l + 1))
        (merge sortedRanges)
    end

  val display1 = Int.toString
  val display2 = IntInf.toString
end

structure Day05: DAY = MakeDay(Day05Impl)
