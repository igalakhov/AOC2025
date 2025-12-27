structure Day08Impl: DAY_IMPL =
struct
  val day = 8

  type state = (IntInf.int * IntInf.int * IntInf.int) list

  type result1 = int
  type result2 = IntInf.int

  val parse = List.map (fn line =>
    case
      List.map (Option.valOf o IntInf.fromString)
        (String.tokens (fn c => c = #",") line)
    of
      [a, b, c] => (a, b, c)
    | _ => raise ParseError)

  fun sortClosest points =
    let
      fun dist (x1, y1, z1) (x2, y2, z2) =
        (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2)
    in
      ((List.map (fn (_, pts) => pts))
       o (Util.sort (fn ((d1, _), (d2, _)) => IntInf.compare (d1, d2))))
        (List.concat (List.tabulate (List.length points, fn idx1 =>
           List.tabulate ((List.length points) - idx1 - 1, fn offset =>
             (let
                val p1 = List.nth (points, idx1)
                val p2 = List.nth (points, idx1 + offset + 1)
              in
                (dist p1 p2, (idx1, idx1 + offset + 1))
              end)))))
    end

  fun merge p1 p2 groups =
    case
      ( List.find (fn group => List.exists (fn e => e = p1) group) groups
      , List.find (fn group => List.exists (fn e => e = p2) group) groups
      )
    of
      (SOME g1, SOME g2) =>
        (if g1 <> g2 then (g1 @ g2) else g2)
        ::
        (List.filter
           (fn group => List.all (fn e => p1 <> e andalso p2 <> e) group) groups)
    | _ => groups

  fun mergeNTimes 0 _ groups = groups
    | mergeNTimes _ [] groups = groups
    | mergeNTimes n ((p1, p2) :: ps) groups =
        mergeNTimes (n - 1) ps (merge p1 p2 groups)

  fun solve1 points =
    let
      fun go 0 groups _ =
            ((List.foldl (op*) 1) o (fn l => List.take (l, 3)) o List.rev
             o (Util.sort Int.compare) o (List.map List.length)) groups
        | go n groups ((idx1, idx2) :: rest) =
            go (n - 1) (merge idx1 idx2 groups) rest
        | go n groups [] = raise Fail "Unexpected"
    in
      go 1000 (List.tabulate (List.length points, fn idx => [idx]))
        (sortClosest points)
    end

  fun solve2 points =
    let
      fun go _ [] = raise Fail "Unexpected"
        | go groups ((idx1, idx2) :: rest) =
            case merge idx1 idx2 groups of
              [_] =>
                (case (List.nth (points, idx1), List.nth (points, idx2)) of
                   ((x1, _, _), (x2, _, _)) => x1 * x2)
            | newGroups => go newGroups rest
    in
      go (List.tabulate (List.length points, fn idx => [idx]))
        (sortClosest points)
    end

  val display1 = Int.toString
  val display2 = IntInf.toString
end

structure Day08: DAY = MakeDay(Day08Impl)
