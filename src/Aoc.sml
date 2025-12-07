val solutions: DaySolution list =
  [ Day01.solution
  , Day02.solution
  , Day03.solution
  , Day04.solution
  , Day05.solution
  , Day06.solution
  ]

fun runSolution sol lines =
  ( print ("Part 1: " ^ ((#solve1 sol) lines) ^ "\n")
  ; print ("Part 2: " ^ ((#solve2 sol) lines) ^ "\n")
  )

fun main () =
  case CommandLine.arguments () of
    ["run", id, inp] =>
      (case List.find (fn sol => (#id sol) = id) solutions of
         NONE => print ("Problem " ^ id ^ " not found\n")
       | SOME sol => runSolution sol (Util.readLines inp))
  | ["list"] =>
      let val ids = List.map #id solutions
      in print (String.concatWith "\n" ids ^ "\n")
      end
  | _ => print "Usage:\n  aoc run <problem> <input>\n  aoc list\n"

val () = main ()
