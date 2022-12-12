module Year2022Day09

open AdventOfCode.Common

type Knot = { X: int; Y: int }

let moveTail head tail =
    match abs (head.X - tail.X) < 2 && abs (head.Y - tail.Y) < 2 with
    | true -> [head; tail]
    | false -> [head; { X = (if head.X = tail.X then head.X else tail.X + (head.X - tail.X) / abs (head.X - tail.X)); Y = (if head.Y = tail.Y then head.Y else tail.Y + (head.Y - tail.Y) / abs (head.Y - tail.Y)) } ]

let moveRopeHead c (knots: Knot list) =
    match c with
    | 'R' -> { knots.Head with X = knots.Head.X + 1 }
    | 'L' -> { knots.Head with X = knots.Head.X - 1 }
    | 'U' -> { knots.Head with Y = knots.Head.Y + 1 }
    | 'D' -> { knots.Head with Y = knots.Head.Y - 1 }
    | _ -> failwith $"invalid move {c}"
    |> (fun x -> x :: knots.Tail)

let rec moveRopeTail (knots: Knot list) =
    let head, tail, remaining = knots.Head, knots.Tail.Head, knots.Tail.Tail
    match remaining with
    | [] -> moveTail head tail
    | _ -> moveTail head tail |> (fun r -> r.Head :: moveRopeTail (r.Tail @ remaining))
    
let moveRope c knots = knots |> moveRopeHead c |> moveRopeTail  

let solve data rope =
    data
    |> Seq.fold (fun acc elem -> seq { for _ in 1 .. snd elem -> moveRope (fst elem) } |> Seq.fold (fun a f -> f (fst a) |> (fun r -> r, (List.last r) :: snd a)) acc) (rope, [])
    |> (fun (_, tailPositions) -> tailPositions |> List.distinct |> List.length)
    
let solvePart1 data = solve data [for _ in 1..2 -> { X = 0; Y = 0 }]

let solvePart2 data = solve data [for _ in 1..10 -> { X = 0; Y = 0 }]

let solver ={ parse = parseEachLine (fun s -> s.Split(' ') |> (fun a -> (a[0][0], int a[1]))); part1 = solvePart1; part2 = solvePart2 }