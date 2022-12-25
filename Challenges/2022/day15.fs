module Year2022Day15

open System
open AdventOfCode.Common

type Boundary = { MaxX: int; MinX: int; MaxY: int; MinY: int; }

let parseSegments lines =
  let step (s: string) =
    match s with
    | Regex @"Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)" [sensorX; sensorY; beaconX; beaconY] -> [('S', int sensorX, int sensorY); ('B', int beaconX, int beaconY)]
    | _ -> failwith "mismatch"
  let beaconsAndSensors = Seq.map step lines
  let maxXY = Seq.fold (fun acc l -> List.fold (fun (maxX, minX, maxY, minY) (_, x, y) -> max maxX x, min minX x, max maxY y, min minY y) acc l) (0, 0, 0, 0) beaconsAndSensors |> (fun (maxX, minX, maxY, minY) -> {MaxX = maxX; MinX = minX; MaxY = maxY; MinY = minY})
  let map = (fun boundary -> Array2D.create (boundary.MaxX - boundary.MinX + 1) (boundary.MaxY - boundary.MinY + 1) '.') maxXY
  beaconsAndSensors |> Seq.iter (fun l -> List.iter (fun (c, x, y) -> map[x - maxXY.MinX, y - maxXY.MinY] <- c) l)
  map, beaconsAndSensors, maxXY

let distance (sx, sy) (bx, by) =
  abs (sx - bx) + abs (sy - by)

let writeMap (map: char[,]) =
  for y in 0 .. (Array2D.length2 map) - 1 do
    for x in 0 .. (Array2D.length1 map) - 1 do
      Console.Write(map[x,y])
    Console.WriteLine()
    
let solvePart1 data =
  let (map: char[,]), bas, maxXY = data
  let definitionToCoordinates d = d |> (fun (_, x, y) -> x - maxXY.MinX, y - maxXY.MinY)
  let eliminateBeacons (sx,sy) dist =
    for y in max 0 (sy - dist) .. min maxXY.MaxY (sy + dist) do
      for x in max 0 (sx - dist) .. min maxXY.MaxX (sx + dist) do
        match distance (x, y) (sx, sy) with
        | d when d <= dist && map[x, y] = '.' -> map[x,y] <- '#'
        | _ -> ()
  writeMap map
  [('S', 8, 7); ('E', 2, 10)] |> (fun (x: (char*int*int) list) -> eliminateBeacons (definitionToCoordinates x.Head) (distance (definitionToCoordinates x.Head) (definitionToCoordinates x.Tail.Head)))
  // bas |> Seq.iter (fun (x: (char*int*int) list) -> eliminateBeacons (definitionToCoordinates x.Head) (distance (definitionToCoordinates x.Head) (definitionToCoordinates x.Tail.Head)))
  writeMap map
  data
  |> (fun (_, bas, _) -> Seq.map (fun (x: (char*int*int) list) -> distance (definitionToCoordinates x.Head) (definitionToCoordinates x.Tail.Head)) bas)
  |> printfn "%A"

let solvePart2 data =
  data
  |> printfn "%A"

let solver = { parse = parseSegments; part1 = solvePart1; part2 = solvePart2 }