open System
open System.IO

type Position = int * int

type FoldInstruction =
    | AlongX of int
    | AlongY of int

let parseDots (line : string) =
    let [| x; y |] = line.Split(',')
    int x, int y

let parseInstruction (line : string) =
    let [| alongPart; positionPart |] = line.Split('=')
    match Seq.last alongPart with
    | 'x' -> AlongX (int positionPart)
    | 'y' -> AlongY (int positionPart)
    | _   -> invalidOp (sprintf "unexpected fold instruction %s" line)

let parse (newLine : string) (input : string) =
    let [| dotsPart; foldInstructionParts |] =
        input.Split(String.replicate 2 newLine, StringSplitOptions.RemoveEmptyEntries)

    let dots =
        dotsPart.Split(newLine, StringSplitOptions.RemoveEmptyEntries)
        |> Array.map parseDots
        |> Set.ofArray

    let instructions =
        foldInstructionParts.Split(newLine, StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map parseInstruction
        |> Seq.toList

    dots, instructions

let foldPosition instruction (x, y) =
    match instruction with
    | AlongY pos ->
        if y > pos then
            let diff = y - pos
            let newY = pos - diff
            (x, newY)
        else
            (x, y)
    | AlongX pos ->
        if x > pos  then
            let diff = x - pos
            let newX = pos - diff
            (newX, y)
        else
            (x, y)

let foldStep dots instruction =
    dots |> Set.map (foldPosition instruction)

let solve dots = foldStep dots >> Set.count

let content =
    File.ReadAllText (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))

let dots, i :: _ = parse Environment.NewLine content
solve dots i