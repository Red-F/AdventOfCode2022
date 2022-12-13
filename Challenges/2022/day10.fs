module Year2022Day10

open System
open AdventOfCode.Common

let execute (op, xStart, cycles, signalStrengths, signalCycle) =
    let processSignalStrength c =
        c |> Seq.fold (fun acc elem -> if cycles + elem  <> signalCycle then acc else  ((cycles + elem) * xStart :: fst acc), signalCycle + 40) (signalStrengths, signalCycle)
    match op with
    | "noop" -> seq { 1 } |> processSignalStrength |> (fun x -> xStart, cycles + 1, fst x, snd x) 
    | _ -> seq { 1; 2 } |> processSignalStrength |> (fun x -> int (op.Split(' ')[1]) + xStart, cycles + 2, fst x, snd x)


let executeSprite (op, xStart, cycles, crt: char array) =
    let draw pixelPosition spritePosition =
        match pixelPosition % 40 with | pos when spritePosition - 1 <= pos && pos <= spritePosition + 1 -> '#' | _ -> '.'
    let processSprite c =
        c |> Seq.iter (fun i -> draw (cycles + i) xStart |> Array.set crt (cycles+i))
    match op with
    | "noop" -> seq { 0 } |> processSprite |> (fun _ -> xStart, cycles + 1, crt) 
    | _ -> seq { 0; 1 } |> processSprite |> (fun _ -> int (op.Split(' ')[1]) + xStart, cycles + 2, crt)

let dumpCrt crt =
    crt |> Array.iteri (fun i v -> if (i+1) % 40 = 0 then Console.WriteLine($"{v}") else Console.Write($"{v}")) |> (fun _ -> "see above")
    
let solvePart1 data =
    data |> Seq.fold (fun (x, cycleCount, signalStrengths, signalCycle) elem -> execute (elem, x, cycleCount, signalStrengths, signalCycle)) (1, 0, [], 20) |> (fun (_, _, signalStrengths, _) -> List.sum signalStrengths)

let solvePart2 data =
    data |> Seq.fold (fun (x, cycleCount, crt) elem -> executeSprite (elem, x, cycleCount, crt)) (1, 0, [|for _ in 1..240 -> ' '|]) |> (fun (_, _, crt) -> dumpCrt crt)

let solver ={ parse = parseEachLine asString; part1 = solvePart1; part2 = solvePart2 }