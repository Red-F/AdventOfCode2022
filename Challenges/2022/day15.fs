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
  member this.Keys =
    table.Keys
  member this.ContainsKey x =
    table.ContainsKey x

type Boundary = { MaxX: int64; MinX: int64; MaxY: int64; MinY: int64 }

let distance (sx, sy) (bx, by) =
  abs (sx - bx) + abs (sy - by)

let parseSegments lines =
  let step (s: string) =
    match s with
    | Regex @"Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)" [sensorX; sensorY; beaconX; beaconY] -> (((int64 sensorX, int64 sensorY), 'S'), ((int64 beaconX, int64 beaconY), 'B'), distance (int64 sensorX, int64 sensorY) (int64 beaconX, int64 beaconY))
    | _ -> failwith "mismatch"
  let beaconsAndSensors = Seq.map step lines |> Seq.sortBy (fun (_, _, dist) -> dist)
  let map = sparseMatrix()
  beaconsAndSensors |> Seq.iter (fun (s,e,_) -> seq {s; e} |> Seq.iter (fun ((x, y), c) -> map[x, y] <- c))
  map, beaconsAndSensors

let writeMap (map: sparseMatrix) =
  let limits = map.Keys |> Seq.head |> (fun a ->  {MinX = fst a; MaxX = fst a; MinY = snd a; MaxY = snd a})
  let limits = map.Keys |> Seq.fold (fun acc (x, y) -> {MinX = min x acc.MinX; MaxX = max x acc.MaxX; MinY = min y acc.MinY; MaxY = max y acc.MaxY}) limits
  for y in limits.MinY .. limits.MaxY do
    for x in limits.MinX .. limits.MaxX do
      Console.Write(if map.ContainsKey (x,y) then map[x,y] else '.')
    Console.WriteLine(match y with |9L |10L | 11L -> string y | _ -> "")
  Console.WriteLine()
  
let writePerimeter s =
  s |> Seq.iter (fun (x,y) -> Console.WriteLine($"({x}, {y})"))

let constructRow (theMap: sparseMatrix) bas row =
  printfn $"row %d{row}"
  let theRow = theMap.Keys |> Seq.filter (fun (_, y) -> y = row) |> Seq.sortBy fst |> Seq.map (fun(x,y) -> ((x,y), theMap[x,y])) |> Map.ofSeq
  let eliminateBeacons (sx,sy) dist currentRow =
    if abs (sy - row) <= dist then
      seq { for x in sx - (dist - (abs (sy - row))) .. sx + (dist - (abs (row - sy))) -> (x, row) }
      |> Seq.fold (fun (acc: Map<int64*int64,char>) (sx,sy) -> if acc.ContainsKey (sx,sy) |> not then acc.Add ((sx,sy), '#') else acc) currentRow
    else
      currentRow
  bas |> Seq.fold (fun acc (((sx,sy),_), _, dist) -> eliminateBeacons (sx,sy) dist acc) theRow
  |> (fun s -> s.Keys |> Seq.sortBy fst, s) |> (fun (keys, s) -> Seq.map (fun (x,y) -> (x,y), s[x,y] ) keys)

let constructPerimeter (sx,sy) dist =
  Console.WriteLine($"({sx}, {sy}) -- {dist}")
  let outside y d =
    match y with
    | _ when y = (sy-dist) -> seq { (sx,y-1L); (sx-1L,y); (sx+1L,y) }
    | _ when y = (sy+dist) -> seq { (sx,y+1L); (sx-1L,y); (sx+1L,y) }
    | _ -> seq { (sx-dist-1L,y); (sx+dist+1L,y) }
  seq { sy - dist .. sy + dist } |> Seq.map (fun y -> outside y (abs (dist - abs (sy - y))))  |> Seq.collect id
  // seq { sy - dist .. sy + dist } |> Seq.map (fun y -> y, abs (dist - abs (sy - y)) )
  // seq { sy - dist .. sy + dist } |> Seq.map (fun y -> abs (dist - y) |> (fun d -> seq { sx - d; sx + d }))
    
let solvePart1 data =
  0L
  // let map, bas = data
  // let targetRow = 2000000L
  // let targetRow = 10L
  // constructRow map bas targetRow |> Seq.filter (fun (_,c) -> c <> 'B') |> Seq.length
  // let eliminateBeacons (sx,sy) dist (theMap: sparseMatrix) targetRow =
  //   printfn $"(%d{sx}, %d{sy}) distance %d{dist}"
  //   if abs (sy - targetRow) <= dist then
  //     seq { for x in sx - (dist - (abs (sy - targetRow))) .. sx + (dist - (abs (targetRow - sy))) -> (x, targetRow) }
  //     |> Seq.iter (fun (x, y) -> if y = targetRow && theMap.ContainsKey (x, y) |> not then theMap[x,y] <- '#')
  // writeMap map
  // bas |> Seq.iter (fun (((sx,sy),_), _, dist) -> eliminateBeacons (sx,sy) dist map targetRow) 
  // writeMap map
  // map.Keys |> Seq.filter (fun (_, y) -> y = targetRow) |> Seq.map (fun(x,y) -> map[x,y]) |> Seq.filter (fun c -> c = '#') |> Seq.length

let solvePart2 data =
  let map, bas = data
  let maxCoordinate = 4000000L
  // let maxCoordinate = 20L
  // let perimeter =  Seq.head bas |> (fun (((sx,sy),_), _, dist) -> constructPerimeter (sx,sy) dist)
  let perimeters = bas |> Seq.map (fun (((sx,sy),_), _, dist) -> constructPerimeter (sx,sy) dist |> Seq.filter (fun (x,y) -> x>=0 && x <= maxCoordinate && y >= 0 && y <= maxCoordinate)) |> Seq.collect id |> Seq.distinct
  // writePerimeter perimeter
  // perimeters |> writePerimeter
  perimeters |> Seq.length |> printfn "%A"
  perimeters |> Seq.distinct |> Seq.filter (fun (x,y) -> x>=0 && x <= maxCoordinate && y >= 0 && y <= maxCoordinate) |> Seq.filter (fun (x,y) -> bas |> Seq.fold (fun acc (((sx,sy),_), _, dist) -> acc && (distance (sx,sy) (x,y)) > dist) true)
  |> Seq.head |> (fun (x,y) -> (x * 4000000L) + y)
  // let map, bas = data
  // seq { for i in 0L .. 4000000L do i }
  // |> Seq.map (constructRow map bas)
  // |> Seq.filter (fun s -> (Seq.head s, Seq.last s, Seq.length s) |> (fun (((xs,_),_), ((xe,_),_), count) -> int64 count <> (xe-xs+1L)))
  // |> Seq.head |> (fun s -> (Seq.head s, s)) |> (fun (((x,y),_),s) -> Seq.fold (fun (ax, ay) ((x,y),_) -> if y = ay then (ax,ay) else if x <> (ax+1L) then (x-1L,y) else (x,ay)) (x-1L,y-1L) s)
  // |> (fun (x,y) -> (4000000L * x) + y)
  // |> printfn "%A"
  // writeMap map
  // map.Keys |> Seq.filter (fun (_, y) -> y = targetRow) |> Seq.map (fun(x,y) -> map[x,y]) |> Seq.filter (fun c -> c = '#') |> Seq.length

let solver = { parse = parseSegments; part1 = solvePart1; part2 = solvePart2 }