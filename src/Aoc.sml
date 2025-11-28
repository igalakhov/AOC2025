val solutions: DaySolution list = [Day01.solution]


fun main () =
  case CommandLine.arguments () of
    "run" :: _ => raise Fail "Not implemented"
  | ["list"] =>
      let val ids = List.map #id solutions
      in print (String.concatWith "\n" ids ^ "\n")
      end
  | _ => print "Usage:\n  aoc run <problem> <input>\n  aoc list\n"

val () = main ()
