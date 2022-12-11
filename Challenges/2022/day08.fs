module Year2022Day08

open AdventOfCode.Common
  
let parseMap data =
  data |> array2D |> Array2D.map  (fun c -> int (c - '0'))
  
let visibleInSlice vector threshold =
  vector |> Array.fold (fun acc elem -> acc && elem < threshold) true

let visible x y (map: int[,]) =
  x = 0 || y = 0 || x = (Array2D.length1 map)-1 || y = (Array2D.length2 map)-1 || visibleInSlice map[x, 0..y-1] map[x,y] ||
  visibleInSlice map[x, y+1..] map[x,y] || visibleInSlice map[0..x-1,y] map[x,y] || visibleInSlice map[x+1..,y] map[x,y]
  
let viewingDistanceInSLice vector threshold =
  vector |> Array.fold (fun (blocked, distance) elem -> if blocked then blocked, distance else elem >= threshold, distance+1 ) (false, 0) |> snd
  
let scenicScore x y (map: int[,]) =
  viewingDistanceInSLice (Array.rev map[x, 0..y-1]) map[x,y] * viewingDistanceInSLice map[x, y+1..] map[x,y]
  * viewingDistanceInSLice (Array.rev map[0..x-1,y]) map[x,y] * viewingDistanceInSLice map[x+1..,y] map[x,y]
  
let solvePart1 (data: int[,])  =
  data |> Array2D.mapi (fun i j _ -> visible i j  data) |> Seq.cast<bool> |> Seq.filter id |> Seq.length
  
let solvePart2 data =
  data |> Array2D.mapi (fun i j _ -> scenicScore i j data) |> Seq.cast<int> |> Seq.max

let solver = { parse = parseMap; part1 = solvePart1; part2 = solvePart2 }