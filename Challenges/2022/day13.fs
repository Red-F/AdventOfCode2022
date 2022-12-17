module Year2022Day13

open AdventOfCode.Common

let parsePairs lines =
  let step (pairs, lastString) (s: string) =
    match s with
    | "" -> pairs, ""
    | _ when lastString = "" -> pairs, s
    | _ when lastString <> "" -> pairs @ [(lastString, s)], ""
    | _ -> failwith "out of range"
  lines |> Seq.fold step ([], "") |> fst |> List.toArray

let rec token s =
  match s with
  | "" -> ("", "")
  | Regex @"^(\[)(.*)" [front; remaining] -> (front, remaining)
  | Regex @"^([0-9]+)(.*)" [front; remaining] -> (front, remaining)
  | Regex @"^(\])(.*)" [front; remaining] -> (front, remaining)
  | Regex @"^,(.*)" [remaining] -> token remaining
  | _ -> failwith $"don't know about {s}"
  
let rec tokens tokenString l =
  match token tokenString with
  | "", "" -> l
  | front, remaining -> tokens remaining (l @ [front]) 

let rec compare (left: string list) (right: string list) =
  match left, right with
  | [], _ -> true
  | _, [] -> false
  | _ ->
  match left.Head, right.Head with
  | "[", "[" | "]", "]" -> compare left.Tail right.Tail
  | "[", "]" | _, "]" -> false 
  | "]", "[" | "]", _  -> true
  | x, "[" -> compare (x :: "]" :: left.Tail) right.Tail 
  | "[", x -> compare left.Tail (x :: "]" :: right.Tail)
  | l, r when int l = int r -> compare left.Tail right.Tail
  | l, r -> int l < int r

let comparePatterns left right = compare (tokens left []) (tokens right []) |> not
  
let sort (arr: string array ) =
  let arr = arr |> Array.copy
  let swap i j = let tmp = arr[i] in arr[i] <- arr[j]; arr[j] <- tmp
  for i = arr.Length - 1 downto 0 do
    for j = 1 to i do
      if (comparePatterns arr[j-1] arr[j]) then swap (j-1) j
  arr
  
let solvePart1 data =
  data |> Array.map (fun (left,right) -> (tokens left [], tokens right []))
  |> Array.mapi (fun i (left, right) -> i+1, compare left right)
  |> Array.fold (fun acc (i, result) -> if result then acc + i else acc ) 0

let solvePart2 data =
  data |> Array.fold (fun acc (left, right) -> left :: right :: acc ) ["[[2]]"; "[[6]]"] |> List.toArray
  |> sort |> Array.mapi (fun i s -> i+1, s) |> Array.fold (fun acc (i, s) -> match s with | "[[2]]" | "[[6]]" -> acc * i | _ -> acc) 1

let solver = { parse = parsePairs; part1 = solvePart1; part2 = solvePart2 }