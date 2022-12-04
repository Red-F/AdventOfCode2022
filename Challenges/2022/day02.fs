module Year2022Day02

open AdventOfCode.Common

let part1Map =
  Map [("A X", 3 + 1); ("A Y", 6 + 2); ("A Z", 0 + 3); ("B X", 0 + 1); ("B Y", 3 + 2); ("B Z", 6 + 3); ("C X", 6 + 1); ("C Y", 0 + 2); ("C Z", 3 + 3); ]
let part2Map =
  Map [("A X", 0 + 3); ("A Y", 3 + 1); ("A Z", 6 + 2); ("B X", 0 + 1); ("B Y", 3 + 2); ("B Z", 6 + 3); ("C X", 0 + 2); ("C Y", 3 + 3); ("C Z", 6 + 1); ]
  
let solvePart1 data  =
  data |> Seq.map (fun x -> part1Map.[x]) |> Seq.sum
  
let solvePart2 data =
  data |> Seq.map (fun x -> part2Map.[x]) |> Seq.sum

let solver =
    { parse = parseEachLine asString; part1 = solvePart1; part2 = solvePart2 }