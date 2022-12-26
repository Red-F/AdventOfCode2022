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

type Boundary = { MaxX: int64; MinX: int64; MaxY: int64; MinY: int64 }

let parseSegments lines =
  let step (s: string) =
    match s with
    | Regex @"Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)" [sensorX; sensorY; beaconX; beaconY] -> [(int64 sensorX, int64 sensorY), 'S'; (int64 beaconX, int64 beaconY), 'B']
    | _ -> failwith "mismatch"
  let beaconsAndSensors = Seq.map step lines
  let theMap = beaconsAndSensors |> Seq.collect List.toSeq |> Map.ofSeq
  let z = theMap[(8L, 7L)]
  let map = sparseMatrix()
  beaconsAndSensors |> Seq.iter (fun l -> List.iter (fun ((x, y), c) -> map[x, y] <- c) l)
  theMap, beaconsAndSensors

let distance (sx, sy) (bx, by) =
  abs (sx - bx) + abs (sy - by)

let writeMap (map: Map<int64*int64,char>) =
  let limits = fst (Map.minKeyValue map) |> (fun a ->  {MinX = fst a; MaxX = fst a; MinY = snd a; MaxY = snd a})
  let limits = map |> Map.fold (fun acc (x, y) v -> {MinX = min x acc.MinX; MaxX = max x acc.MaxX; MinY = min y acc.MinY; MaxY = max y acc.MaxY}) limits
  for y in limits.MinY .. limits.MaxY do
    for x in limits.MinX .. limits.MaxX do
      Console.Write(if map.ContainsKey (x,y) then map[x,y] else '.')
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
  writeMap map
  map 
  |> printfn "%A"

let solvePart2 data =
  data
  |> printfn "%A"

let solver = { parse = parseSegments; part1 = solvePart1; part2 = solvePart2 }