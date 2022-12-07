module Year2022Day05

open AdventOfCode.Common

type Move = { Count: int; From: int; To: int }

let parseMap data =
    data |> List.map (fun s -> Seq.chunkBySize 4 s |> Seq.map (fun x -> match x[1] with |' ' -> [' '] |_ -> [x[1]]))
    |> List.reduce (fun acc elem -> Seq.map2 (fun a b -> match b with |[' '] -> a |_ -> List.append  b a) elem acc) |> Seq.toArray

let parseZ lines =
  let step (map, moves) s =
    match s with
    | "" -> (map, moves)
    | _ when s.TrimStart()[0] = '1' -> (map, moves)
    | Regex @"[^0-9]+([0-9]+)[^0-9]+([0-9]+)[^0-9]+([0-9]+)" [count; fromStack; toStack; ] -> (map, List.append moves [{ Count =  int count; From = int fromStack; To = int toStack }])
    | _ -> (List.append map [s], moves)
  Seq.fold step ([], []) lines |> (fun (a, c) -> (parseMap a, c))
    
let rec moveOne (stacks: char list[]) count fromStack toStack =
  match count with
  | 0 -> stacks
  | _ ->
    stacks[toStack] <- List.append [match stacks[fromStack] with | head::_ -> head | [] -> invalidArg "fromStack" "isEmpty"] stacks[toStack]
    stacks[fromStack] <- match stacks[fromStack] with | _::tail -> tail | [] -> invalidArg "fromStack" "isEmpty"
    moveOne stacks (count-1) fromStack toStack

let rec moveMany (stacks: char list[]) count fromStack toStack =
  let moving, remaining = List.splitAt count stacks[fromStack]
  stacks[toStack] <- List.append moving stacks[toStack]
  stacks[fromStack] <-remaining
  stacks

let solvePart1 data  =
  data |> (fun (a, b) -> List.fold (fun acc elem -> moveOne acc elem.Count (elem.From-1) (elem.To-1)) a b) |> Array.fold (fun acc elem -> acc + string elem.Head) ""
  
let solvePart2 data =
  data |> (fun (a, b) -> List.fold (fun acc elem -> moveMany acc elem.Count (elem.From-1) (elem.To-1)) a b) |> Array.fold (fun acc elem -> acc + string elem.Head) ""

let solver = { parse = parseZ; part1 = solvePart1; part2 = solvePart2 }