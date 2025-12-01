structure Util =
struct
  val stoi = Option.valOf o Int.fromString

  fun splitBy c =
    String.fields (fn c' => c' = c)

  val sum = foldl op+ 0
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
