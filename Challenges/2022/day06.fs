module Year2022Day06

open AdventOfCode.Common

let searchMarker size data =
  data |> Seq.head |> Seq.windowed size
  |> Seq.fold (fun (count, found) elem -> match (found, (Array.distinct elem).Length) with |true, _ -> (count, true)| false, x when x = size -> (count, true) |false, _ -> (count+1, false)) (0, false)
  |> (fun (count, _) -> count+size)
  
let solvePart1 data  =
  data |> searchMarker 4
  
let solvePart2 data =
  data |> searchMarker 14

let solver = { parse = parseEachLine asString; part1 = solvePart1; part2 = solvePart2 }