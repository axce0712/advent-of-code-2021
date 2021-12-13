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

let print dots =
    let minX = dots |> Set.map fst |> Set.minElement
    let minY = dots |> Set.map snd |> Set.minElement
    let maxX = dots |> Set.map fst |> Set.maxElement
    let maxY = dots |> Set.map snd |> Set.maxElement
    "\n" + String.concat "\n" [
        for y in minY..maxY do
            String.concat "" [
                for x in minX..maxX do
                    if Set.exists ((=) (x, y)) dots then "#" else "."
            ]
    ]

let content =
    File.ReadAllText (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))

parse Environment.NewLine content
||> List.fold foldStep
|> print