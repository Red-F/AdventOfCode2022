namespace AdventOfCode

open System
open System.Text.RegularExpressions

// This is a set of methods that I found at https://github.com/CameronAavik/AdventOfCode
// 
module Common =
    // Every day has a corresponding Day record which defines how to parse the file
    // then two functions for soving each part respectively
    type Day<'a, 'b, 'c> = { parse: string seq -> 'a; part1: 'a -> 'b; part2: 'a -> 'c }

    // helper methods for parsing
    let parseFirstLine f = Seq.head >> f
    let parseEachLine = Seq.map
    let parseEachLineIndexed = Seq.mapi
    let asString : string -> string = id
    let asInt : string -> int = int
    let asStringArray : string [] -> string [] = Array.map string
    let asIntArray : string [] -> int [] = Array.map int
    let splitBy (c : string) f (str : string) = str.Split([| c |], StringSplitOptions.None) |> f
    let (|Regex|_|) pattern input = Regex.Match(input, pattern) |> (fun m -> if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ]) else None)
