module Year2022Day01

open AdventOfCode.Common

let parseElves lines =
  let step (elves, currentElf) s =
    match s with
    | "" -> (List.rev currentElf :: elves), []
    | _ -> elves, (s |> int) :: currentElf
    
  let elves, lastElf = Seq.fold step ([], []) lines
  
  List.rev (lastElf :: elves)

let solvePart1 data  =
  data |> parseElves |> Seq.map Seq.sum |> Seq.max

let solvePart2 data =
  data |> parseElves |> Seq.map Seq.sum |> Seq.sortDescending |> Seq.take 3 |> Seq.sum 
  
let solver =
    { parse = parseEachLine asString ; part1 = solvePart1; part2 = solvePart2 }
