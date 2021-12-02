open System
open System.IO

type Position = { Horizontal : int; Depth : int; Aim : int }

type Instruction =
    | Forward of int
    | Down of int
    | Up of int

let (|Int|_|) (input : string) =
    match Int32.TryParse input with
    | true, result -> Some result
    | false, _ -> None

let parseLine (line : string) =
    match line.Split(' ', StringSplitOptions.None) with
    | [| "forward"; Int value |] -> Forward value
    | [| "down"; Int value |] -> Down value
    | [| "up"; Int value |] -> Up value
    | _ -> failwithf "can't handle line '%s'" line

let update positionSoFar instruction =
    match instruction with
    | Down x -> { positionSoFar with Aim = positionSoFar.Aim + x }
    | Up x -> { positionSoFar with Aim = positionSoFar.Aim - x }
    | Forward x ->
        { positionSoFar with
            Horizontal = positionSoFar.Horizontal + x
            Depth = positionSoFar.Depth + positionSoFar.Aim * x }

let instructions =
    File.ReadLines (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))
    |> Seq.map parseLine

let finalPosition =
    instructions
    |> Seq.fold update { Horizontal = 0; Depth = 0; Aim = 0 }

finalPosition.Horizontal * finalPosition.Depth