structure Day06Impl: DAY_IMPL =
struct
  val day = 6

  datatype operation = ADD | MULT
  type state = (string list * operation) list

  type result1 = IntInf.int
  type result2 = IntInf.int

  val stoi = Option.valOf o IntInf.fromString

  fun parse lines =
    let
      fun numSpaces [] = (0, [])
        | numSpaces (#" " :: rest) =
            (fn (ns, lst) => (ns + 1, lst)) (numSpaces rest)
        | numSpaces lst = (0, lst)
      fun go ([], _) = []
        | go (ops, nums) =
            let
              val (curOp, restOps) = (Option.valOf o List.getItem) ops
              val (numChars, restOps) = numSpaces restOps
            in
              (( (List.map
                    (fn lst => String.implode (List.take (lst, numChars + 1)))
                    nums)
               , (case curOp of
                    #"+" => ADD
                  | #"*" => MULT
                  | _ => raise ParseError)
               )
               ::
               (go
                  ( restOps
                  , (List.map (fn lst => List.drop (lst, numChars + 1)) nums)
                  )))
            end
      val (ops, nums) = (Option.valOf o List.getItem o List.rev) lines
    in
      go (String.explode ops, (List.rev o (List.map String.explode)) nums)
    end

  val solveProblems =
    let
      fun eval (nums, ADD) =
            List.foldl (op+) 0 (List.map stoi nums)
        | eval (nums, MULT) =
            List.foldl (op*) 1 (List.map stoi nums)
    in
      (List.foldl (op IntInf.+) 0) o (List.map eval)
    end

  fun transposeProblem (nums, operation) =
    let
      fun transposeNums nums =
        if List.all List.null nums then
          []
        else
          (((String.implode
             o
             (List.mapPartial (fn num =>
                case List.hd num of
                  #" " => NONE
                | c => SOME c))) nums)
           :: (transposeNums (List.map List.tl nums)))
    in
      ( List.filter
          (fn num => (not (List.all (fn c => c = #" ") (String.explode num))))
          (transposeNums (List.map String.explode nums))
      , operation
      )
    end

  val solve1 = solveProblems

  val solve2 = solveProblems o (List.map transposeProblem)

  val display1 = IntInf.toString
  val display2 = IntInf.toString
end

structure Day06: DAY = MakeDay(Day06Impl)
