module Year2022Day11

open AdventOfCode.Common

type Operation = {Operator: char; Operand: int64}    
type Monkey = {Nr : int; Items: int64 list; Op: Operation; Divisor: int64; IfTrue: int; IfFalse: int}
with static member Empty = {Nr = 0; Items = []; Op = {Operator = ' '; Operand = 0}; Divisor = 0; IfTrue = 0; IfFalse = 0}

let parseMonkeys lines =
  let parseMonkey ml =
     let parseMonkeyLine l monkey =
         match l with
         | Regex @"Monkey (.*):" [m] -> {monkey with Nr = int m}
         | Regex @"  Starting items: (.*)" [m] -> {monkey with Items = m.Split(", ") |> Array.map int64 |> List.ofArray}
         | Regex @"  Operation: new = old \* old" [] -> {monkey with Op = {Operator = '2'; Operand = 0}}
         | Regex @"  Operation: new = old (.) (.*)" [m; l] -> {monkey with Op = {Operator = m[0]; Operand = int64 l}}
         | Regex @"  Test: divisible by (.*)" [m] -> {monkey with Divisor = int64 m}
         | Regex @"    If true: throw to monkey (.*)" [m] -> {monkey with IfTrue = int m}
         | Regex @"    If false: throw to monkey (.*)" [m] -> {monkey with IfFalse = int m}
         | _ -> failwith $"unknown statement: {l}"
     ml |> List.fold (fun acc elem -> parseMonkeyLine elem acc) Monkey.Empty 
  let step (monkeys, currentMonkey) s =
    match s with
    | "" -> (monkeys @ [currentMonkey] ), []
    | _ -> monkeys, currentMonkey @ [s]
  lines |> Seq.fold step ([], []) |> (fun (monkeys, lastMonkey) -> monkeys @ [lastMonkey]) |> List.map parseMonkey |> List.toArray

let turn (monkey: Monkey) (monkeys: Monkey array) relief =
    let handle m wl =
        let inspect m wl = match m.Op.Operator with | '2' -> wl * wl | '*' -> wl * m.Op.Operand | '+' -> wl + m.Op.Operand | _ -> failwith "invalid operation"
        let throw nr wl = monkeys[nr] <- {monkeys[nr] with Items = monkeys[nr].Items @ [wl]}
        inspect m wl |> relief |> (fun (x: int64) -> match x % m.Divisor = 0 with | true -> throw m.IfTrue x | false -> throw m.IfFalse x) 
    monkey.Items |> List.fold (fun acc elem -> handle acc elem |> (fun _ -> {acc with Items = acc.Items.Tail})) monkey |> (fun m -> monkeys[m.Nr] <- m)

let round (monkeys: Monkey array) relief =
    monkeys |> Array.fold (fun acc elem -> turn elem monkeys relief |> (fun _ -> Array.append acc [|int64 elem.Items.Length|])) [||]

let rounds count relief (monkeys: Monkey array) =
    seq { 1 .. count } |> Seq.fold (fun inspections _ -> round monkeys relief |> (fun x -> inspections @ [x])) []

let solvePart1 data =
    data |> parseMonkeys |> rounds 20 (fun x -> x / 3L)|> (fun x -> List.fold (fun acc elem -> Array.mapi2 (fun _ x y -> x+y) elem acc)  x.Head x.Tail)
    |> Array.sortDescending |> (fun a -> a[0]*a[1]) 

let solvePart2 data =
    data |> parseMonkeys |> (fun a -> Array.fold (fun (monkeys, lcd) elem -> (monkeys, lcd * elem.Divisor)) (a, 1L) a)
    |> (fun (monkeys, lcd) -> rounds 10000 (fun x -> x % lcd) monkeys) |> (fun x -> List.fold (fun acc elem -> Array.mapi2 (fun _ x y -> x+y) elem acc)  x.Head x.Tail)
    |> Array.sortDescending |> (fun a -> a[0]*a[1]) 

let solver ={ parse = parseEachLine asString; part1 = solvePart1; part2 = solvePart2 }