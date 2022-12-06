module Year2022Day04

open AdventOfCode.Common

let parsePairs (s : string) =
  s.Split(',') |> Array.map (fun x -> x.Split('-') |> (fun x -> seq {int x[0] .. int x[1]} |> Set.ofSeq)) 
  
let solvePart1 data  =
  data |> Seq.filter (fun (x: Set<int>[]) -> (Set.isSubset x[0] x[1]) || (Set.isSubset x[1] x[0])) |> Seq.length
  
let solvePart2 data =
  data |> Seq.filter (fun (x: Set<int>[]) -> not (Set.isEmpty (Set.intersect x[0] x[1]))) |> Seq.length

let solver = { parse = parseEachLine parsePairs; part1 = solvePart1; part2 = solvePart2 }