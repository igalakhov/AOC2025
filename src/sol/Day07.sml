structure Day07Impl: DAY_IMPL =
struct
  val day = 7

  datatype elem = BEAM of IntInf.int | SPLITTER
  type state = elem Array2.array

  type result1 = IntInf.int
  type result2 = IntInf.int

  val parse =
    Array2.fromList
    o
    (List.map
       ((List.map (fn c =>
           case c of
             #"." => BEAM 0
           | #"^" => SPLITTER
           | #"S" => BEAM 1
           | _ => raise ParseError)) o String.explode))

  fun getAns grid =
    let
      val numSplits = ref (IntInf.fromInt 0)
      val (numRows, numCols) = Array2.dimensions grid
      fun updateRow rowIdx =
        let
          fun addBeam colIdx n' =
            let
              val current = Array2.sub (grid, rowIdx, colIdx)
            in
              case current of
                BEAM n => Array2.update (grid, rowIdx, colIdx, BEAM (n + n'))
              | SPLITTER => ()
            end
        in
          List.app
            (fn colIdx =>
               (case
                  ( Array2.sub (grid, rowIdx - 1, colIdx)
                  , Array2.sub (grid, rowIdx, colIdx)
                  )
                of
                  (BEAM n, BEAM _) => addBeam colIdx n
                | (BEAM n, SPLITTER) =>
                    (if n > 0 then
                       ( numSplits := !numSplits + 1
                       ; addBeam (colIdx - 1) n
                       ; addBeam (colIdx + 1) n
                       )
                     else
                       ())
                | _ => ())) (List.tabulate (numCols, fn colIdx => colIdx))
        end
    in
      ( (List.app updateRow (List.tabulate (numRows - 1, fn idx => idx + 1)))
      ; ( !numSplits
        , (Vector.foldl
             (fn (e, acc) =>
                case e of
                  BEAM n => acc + n
                | SPLITTER => acc) 0 (Array2.row (grid, numRows - 1)))
        )
      )
    end

  val solve1 = (fn (ans, _) => ans) o getAns
  val solve2 = (fn (_, ans) => ans) o getAns

  val display1 = IntInf.toString
  val display2 = IntInf.toString
end

structure Day07: DAY = MakeDay(Day07Impl)
