open System
open System.IO

type Position = int * int

type Line = { Begin : Position; End : Position }

let (|Int|_|) (input : string) =
    match Int32.TryParse input with
    | true, result -> Some result
    | false, _ -> None

let (|Position|) (input : string) =
    let [| Int x; Int y |] = input.Split(',')
    (x, y)

let parseLine (line : string) =
    let [| Position b; Position e |] =
        line.Split([| "->" |], StringSplitOptions.None)

    { Begin = b; End = e }

let parseContent (newLine : string) (input : string) =
    input.Split([| newLine |], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map parseLine
    |> Seq.toList

let isHorizontalOrVerticalLine { Begin = (x1, y1); End = (x2, y2) } =
    x1 = x2 || y1 = y2

let generate a b =
    [ for n in min a b .. max a b -> n ]

let pointsCovered { Begin = (x1, y1); End = (x2, y2) } =
    [
        for x in generate x1 x2 do
            for y in generate y1 y2 do
                (x, y)
    ]

let cover m position =
    match Map.tryFind position m with
    | Some n -> Map.add position (n + 1) m
    | None -> Map.add position 1 m

// let content =
//     "0,9 -> 5,9
// 8,0 -> 0,8
// 9,4 -> 3,4
// 2,2 -> 2,1
// 7,0 -> 7,4
// 6,4 -> 2,0
// 0,9 -> 2,9
// 3,4 -> 1,4
// 0,0 -> 8,8
// 5,5 -> 8,2"

// let lines =
//     parseContent "\n" content

let lines = 
    File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
    |> parseContent Environment.NewLine

let covered =
    lines
    |> List.filter isHorizontalOrVerticalLine
    |> List.collect pointsCovered
    |> List.fold cover Map.empty

covered
|> Map.filter (fun _ count -> count > 1)
|> Seq.length