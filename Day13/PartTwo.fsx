open System
open System.IO

type Position = int * int

type FoldInstruction =
    | AlongX of int
    | AlongY of int

type TransparentPaper =
    { Dots : Set<Position>
      FoldInstructions : FoldInstruction list }

fsi.AddPrinter<TransparentPaper>(fun paper ->
    let minX = paper.Dots |> Set.map fst |> Set.minElement
    let minY = paper.Dots |> Set.map snd |> Set.minElement
    let maxX = paper.Dots |> Set.map fst |> Set.maxElement
    let maxY = paper.Dots |> Set.map snd |> Set.maxElement
    "\n" + String.concat "\n" [
        for y in minY..maxY do
            String.concat "" [
                for x in minX..maxX do
                    if Set.exists ((=) (x, y)) paper.Dots then "#" else "."
            ]
    ])

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

    { Dots = dots; FoldInstructions = instructions }

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

let foldStep paper =
    match paper.FoldInstructions with
    | i :: is ->
        let newDots =
            paper.Dots |> Set.map (foldPosition i)

        { paper with
            Dots = newDots
            FoldInstructions = is }
    | [] -> paper

let fold paper =
    let mutable acc = paper
    while acc.FoldInstructions.Length <> 0 do
        acc <- foldStep acc
    acc

let content =
    File.ReadAllText (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))

let paper = parse Environment.NewLine content
fold paper