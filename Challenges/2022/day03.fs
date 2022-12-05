module Year2022Day03

open System
open AdventOfCode.Common

let priority x = if Char.IsLower x then  (int (x - 'a') + 1) else int (x - 'A') + 27
  
let solvePart1 data  =
  data |> Seq.map (fun (x : string) -> x[..(x.Length/2)-1], x[x.Length/2..])
  |> Seq.map (fun (a, b) -> Set.intersect (Set.ofSeq a) (Set.ofSeq b) |> Set.toSeq |> Seq.head |> priority)
  |> Seq.sum
  
let solvePart2 data =
  data |> Seq.chunkBySize 3
  |> Seq.map (fun x -> Set.intersectMany [Set.ofSeq x[0]; Set.ofSeq x[1]; Set.ofSeq x[2]] |> Set.toSeq |> Seq.head |> priority)
  |> Seq.sum

let solver = { parse = parseEachLine asString; part1 = solvePart1; part2 = solvePart2 }