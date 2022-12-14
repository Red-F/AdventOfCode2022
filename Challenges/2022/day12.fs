module Year2022Day12

open System.Collections.Generic
open AdventOfCode.Common

let parseMap data =
  let heightMap, start, finish =
    data |> array2D |>
    (fun a -> (a, Array2D.mapi (fun i j v -> match v with | 'S' -> ('S', i, j)| 'E' -> ('E', i, j) | _-> (' ', 0, 0)) a |> Seq.cast<char * int * int> |> Seq.filter (fun (c, _, _) -> c = 'S' || c = 'E')))
    |> (fun (a, points) -> points |> Seq.iter (fun (c, i, j) -> a[i,j] <- match c with | 'S' -> 'a' | 'E' -> 'z' | _ -> failwith "out of range")  |> (fun _ -> (a, points)))
    |> (fun (a, points) -> a, Seq.head points, Seq.tail points |> Seq.head)
  (heightMap, start, finish)

let neighbors map (x,y) =
    [(0,1); (1, 0); (0, -1); (-1, 0)] |> List.map (fun (dx, dy) -> (x + dx, y + dy))
    |> List.filter (fun (newX, newY) -> newX >= 0 && newY >= 0 && newX < Array2D.length1 map && newY < Array2D.length2 map)
    |> List.filter (fun (newX, newY) -> (int map[newX, newY] - int map[x, y]) <= 1) |> List.map (fun key -> (key, 1))
    
let findPath start finish map =
    let frontier = PriorityQueue<int * int, int>()
    frontier.Enqueue (start, 0)
    let costs = Dictionary<int * int, int>()
    costs[start] <- 0
    while frontier.Count <> 0 do
        let current = frontier.Dequeue()
        if current = finish
        then ()
        else for next, cost in current |> neighbors map do
                 let newCost = costs[current] + cost
                 if costs.ContainsKey(next) |> not || newCost < costs[next] then
                     costs[next] <- newCost
                     let priority = newCost
                     frontier.Enqueue(next, priority)
    if costs.ContainsKey(finish) then costs[finish] else System.Int32.MaxValue
    
let solvePart1 data =
    data
    |> (fun (a, (_, startX, startY), (_, finishX, finishY)) -> findPath (startX, startY) (finishX, finishY) a)

let solvePart2 data =
    let starts = (fun (a, _, _) -> Array2D.mapi (fun i j v -> (v, i, j)) a |> Seq.cast<char * int * int> |> Seq.filter (fun (c, _, _) -> c = 'a')) data |> Seq.map (fun (_, x, y) -> (x, y))
    let map, finish = (fun (a, _, (_, finishX, finishY)) -> (a, (finishX, finishY))) data
    starts |> Seq.map (fun start -> findPath start finish map) |> Seq.min

let solver = { parse = parseMap; part1 = solvePart1; part2 = solvePart2 }