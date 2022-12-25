module Year2022Day15

open System
open System.Collections.Generic
open AdventOfCode.Common

type sparseMatrix() =
  let table = Dictionary<int64 * int64, char>()
  // let table = Dictionary<(int64 * int64), char>()
  member _.Item
    // Because the key is comprised of two values, 'get' has two index values
    with get(key1, key2) = table[(key1, key2)]
    // 'set' has two index values and a new value to place in the key's position
    and set (key1, key2) value = table[(key1, key2)] <- value

type Boundary = { MaxX: int; MinX: int; MaxY: int; MinY: int; }

let parseSegments lines =
  let step (s: string) =
    match s with
    | Regex @"Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)" [sensorX; sensorY; beaconX; beaconY] -> [('S', int64 sensorX, int64 sensorY); ('B', int64 beaconX, int64 beaconY)]
    | _ -> failwith "mismatch"
  let beaconsAndSensors = Seq.map step lines
  let map = sparseMatrix()
  beaconsAndSensors |> Seq.iter (fun l -> List.iter (fun (c, x, y) -> map[x, y] <- c) l)
  map, beaconsAndSensors

let distance (sx, sy) (bx, by) =
  abs (sx - bx) + abs (sy - by)

let writeMap (map: sparseMatrix) =
  
  for y in 0 .. (Array2D.length2 map) - 1 do
    for x in 0 .. (Array2D.length1 map) - 1 do
      Console.Write(map[x,y])
    Console.WriteLine()
    
let solvePart1 data =
  let map, bas = data
  // let definitionToCoordinates d = d |> (fun (_, x, y) -> x - maxXY.MinX, y - maxXY.MinY)
  // let eliminateBeacons (sx,sy) dist =
  //   for y in max 0 (sy - dist) .. min maxXY.MaxY (sy + dist) do
  //     for x in max 0 (sx - dist) .. min maxXY.MaxX (sx + dist) do
  //       match distance (x, y) (sx, sy) with
  //       | d when d <= dist && map[x, y] = '.' -> map[x,y] <- '#'
  //       | _ -> ()
  // writeMap map
  // [('S', 8, 7); ('E', 2, 10)] |> (fun (x: (char*int*int) list) -> eliminateBeacons (definitionToCoordinates x.Head) (distance (definitionToCoordinates x.Head) (definitionToCoordinates x.Tail.Head)))
  // bas |> Seq.iter (fun (x: (char*int*int) list) -> eliminateBeacons (definitionToCoordinates x.Head) (distance (definitionToCoordinates x.Head) (definitionToCoordinates x.Tail.Head)))
  // writeMap map
  // data
  // |> (fun (_, bas, _) -> Seq.map (fun (x: (char*int*int) list) -> distance (definitionToCoordinates x.Head) (definitionToCoordinates x.Tail.Head)) bas)
  map
  |> printfn "%A"

let solvePart2 data =
  data
  |> printfn "%A"

let solver = { parse = parseSegments; part1 = solvePart1; part2 = solvePart2 }