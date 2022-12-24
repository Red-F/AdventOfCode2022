module Year2022Day14

open System
open AdventOfCode.Common

let parseSegments lines =
  let step (s: string) = s.Split(" -> ") |> Array.map (fun s -> s.Split(",") |> (fun a -> int a[1], int a[0]))
  let segments = Seq.map step lines
  let maxXY = Seq.fold (Array.fold (fun a (x,y) -> max (a |> fst) x, max (a |> snd) y))  (0,0) segments
  let map = Array2D.create ((maxXY |> fst) + 3) ((maxXY |> snd) + 500) '.'
  let draw (xStart, yStart) (xEnd, yEnd) =
    match xStart = xEnd with
    | true -> for i in min yStart yEnd .. max yStart yEnd do map[xStart, i] <- '#'
    | false -> for i in min xStart xEnd .. max xStart xEnd do map[i, yStart] <- '#'
  for i in 0 .. (Array2D.length2 map) - 1 do map[(Array2D.length1 map) - 1, i] <- '#'
  segments |> Seq.iter (fun a -> Array.windowed 2 a |> Array.iter (fun b -> draw b[0] b[1])) |> (fun _ -> (maxXY |> fst, map))

let dropSand isPart1 maxY (map: char[,]) =
  let rec moveSand x y =
    match x, y with
    | _ when y = maxY && isPart1 -> true
    | _ when map[y+1, x] = '.' -> moveSand x (y+1)
    | _ when map[y+1, x-1] = '.' -> moveSand (x-1) (y+1)
    | _ when map[y+1, x+1] = '.' -> moveSand (x+1) (y+1)
    | _ when map[0,500] = 'o' && (isPart1 |> not) -> true
    | _ -> (map[y,x] <- 'o') |> (fun _ -> false)
  moveSand 500 0

let solvePart1 data =
  Seq.initInfinite id |> Seq.filter (fun _ -> dropSand true (data |> fst) (data |> snd)) |> Seq.head

let solvePart2 data =
  Seq.initInfinite id |> Seq.filter (fun _ -> dropSand false (data |> fst) (data |> snd)) |> Seq.head

let solver = { parse = parseSegments; part1 = solvePart1; part2 = solvePart2 }