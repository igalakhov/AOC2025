structure Util =
struct
  val stoi = Option.valOf o Int.fromString

  fun splitBy c =
    String.fields (fn c' => c' = c)

  val sum = foldl op+ 0

  fun sort cmp [] = []
    | sort cmp [x] = [x]
    | sort cmp l =
        let
          val half = (List.length l) div 2
          fun merge [] ys = ys
            | merge xs [] = xs
            | merge (x :: xs) (y :: ys) =
                (case cmp (x, y) of
                   LESS => x :: merge xs (y :: ys)
                 | _ => y :: merge (x :: xs) ys)
        in
          merge (sort cmp (List.take (l, half))) (sort cmp
            (List.drop (l, half)))
        end

  fun dedup [] = []
    | dedup [x] = [x]
    | dedup (x :: x' :: xs) =
        if x = x' then dedup (x :: xs) else x :: dedup (x' :: xs)

  fun count pred =
    List.length o (List.filter pred)

  fun printList fmt l =
    print ("[" ^ (String.concatWith ", " (List.map fmt l)) ^ "]\n");

  fun readLines file =
    let
      val ins = TextIO.openIn file
      fun readLines' acc =
        case TextIO.inputLine ins of
          SOME line =>
            (String.substring (line, 0, size line - 1)) :: readLines' acc
        | NONE => acc
      val lines = readLines' []
      val _ = TextIO.closeIn ins
    in
      lines
    end
end
