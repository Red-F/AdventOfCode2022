module Year2022Day07

open AdventOfCode.Common

type Directory =
  { Name: string; mutable Size: int64; Parent: Option<Directory>; mutable Directories: Directory list }
  member this.Root =
    let rec Top (x: Directory) = match x.Parent with | None -> x | Some p -> Top p 
    Top this
    
let rec directorySize (directory: Directory) =
  match directory.Directories with
  | [] -> directory.Size
  | x ->  x |> List.fold (fun acc elem-> acc + directorySize elem) directory.Size

let rec directories (directory: Directory) =
  match directory.Directories with
  | [] -> [directory]
  | _ -> directory :: List.fold (fun acc elem -> acc @ directories elem) [] directory.Directories 
  
let handleTerminalOutput line (currentDirectory: Directory) =
  match line with
  | Regex @"\$ cd (.*)" [dir] ->
    match dir with
    | "/" -> currentDirectory
    | ".." -> match currentDirectory.Parent with | None -> currentDirectory | Some _ -> currentDirectory.Parent.Value
    | x -> currentDirectory.Directories |> List.filter (fun d -> d.Name = x) |> List.head
  | Regex @"([0-9]+) (.*)" [size; _] -> currentDirectory.Size <- currentDirectory.Size + int64 size; currentDirectory
  | Regex @"dir (.*)" [name] -> currentDirectory.Directories <- {Name = name; Parent = Some(currentDirectory); Size = 0L; Directories = []} :: currentDirectory.Directories; currentDirectory
  | _ -> currentDirectory
  
let solvePart1 data  =
  data |> Seq.fold (fun acc elem -> handleTerminalOutput elem acc) { Name = "/"; Parent = None; Size = 0L; Directories = [] }
  |> (fun x -> x.Root) |> directories |> List.map directorySize |> List.filter (fun x -> x <= 100000) |> List.sum
  
let solvePart2 data =
  let root = data |> Seq.fold (fun acc elem -> handleTerminalOutput elem acc) { Name = "/"; Parent = None; Size = 0L; Directories = [] } |> (fun x -> x.Root)
  let requiredSpace = 30000000L - (70000000L - directorySize root)
  root |> directories |> List.map directorySize |> List.sort |> List.filter (fun x -> x >= requiredSpace) |> List.head

let solver = { parse = parseEachLine asString; part1 = solvePart1; part2 = solvePart2 }