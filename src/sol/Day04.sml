structure Day04Impl: DAY_IMPL =
struct
  val day = 4

  type state = bool Array2.array

  type result1 = int
  type result2 = int

  val parse =
    Array2.fromList o List.map (List.map (fn c => c = #"@") o String.explode)

  fun removable grid =
    let
      val dirs = [~1, 0, 1]
      val (rows, cols) = Array2.dimensions grid

      fun inbounds (x, y) =
        0 <= x andalso x < rows andalso 0 <= y andalso y < cols

      fun neighbors x y =
        (List.length
         o
         List.filter ((fn (x', y') =>
           inbounds (x', y') andalso Array2.sub (grid, x', y'))) o List.concat
         o
         List.map (fn dx =>
           List.mapPartial
             (fn dy =>
                if (dx, dy) <> (0, 0) then SOME (x + dx, y + dy) else NONE) dirs))
          dirs
    in
      Array2.foldi Array2.RowMajor
        (fn (x, y, v, acc) =>
           if v andalso (neighbors x y) < 4 then (x, y) :: acc else acc) []
        {base = grid, row = 0, col = 0, nrows = NONE, ncols = NONE}
    end

  val solve1 = List.length o removable

  fun solve2 grid =
    let
      fun go () =
        let
          val toRemove = removable grid
          val () =
            List.app (fn (x, y) => (Array2.update (grid, x, y, false))) toRemove
        in
          if List.null toRemove then 0 else (List.length toRemove) + go ()
        end
    in
      go ()
    end

  val display1 = Int.toString
  val display2 = Int.toString
end

structure Day04: DAY = MakeDay(Day04Impl)
