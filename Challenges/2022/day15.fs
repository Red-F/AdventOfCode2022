module Year2022Day15

open AdventOfCode.Common

let parseSegments lines =
  let step (s: string) =
    match s with
    | Regex @"Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)" [sensorX; sensorY; beaconX; beaconY] -> ((int sensorX, int sensorY), (int beaconX, int beaconY))
    | _ -> failwith "mismatch"
  lines |> Seq.map step

let solvePart1 data =
  data
  |> printfn "%A"

let solvePart2 data =
  data
  |> printfn "%A"

let solver = { parse = parseSegments; part1 = solvePart1; part2 = solvePart2 }