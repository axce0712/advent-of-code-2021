open System
open System.IO

type Position = int * int

type Line = { Begin : Position; End : Position }

type Diagram = Map<Position, int>

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

let generate a b =
    let rec imp acc n =
        if n < b then imp ((n + 1) :: acc) (n + 1)
        elif n > b then imp ((n - 1) :: acc) (n - 1)
        else List.rev acc

    imp [ a ] a

let pointsCovered { Begin = (x1, y1); End = (x2, y2) } =
    if x1 = x2 || y1 = y2
    then
        [
            for x in generate x1 x2 do
                for y in generate y1 y2 do
                    (x, y)
        ]
    else
        List.zip (generate x1 x2) (generate y1 y2)

let cover m position =
    match Map.tryFind position m with
    | Some n -> Map.add position (n + 1) m
    | None -> Map.add position 1 m

let sprintDiagram (m : Diagram) =
    let minX = Map.keys m |> Seq.map fst |> Seq.min
    let minY = Map.keys m |> Seq.map snd |> Seq.min
    let maxX = Map.keys m |> Seq.map fst |> Seq.max
    let maxY = Map.keys m |> Seq.map snd |> Seq.max
    String.concat "\n" [
        for y in minY .. maxY ->
            String.concat "" [
                for x in minX .. maxX ->
                    m |> Map.tryFind (x, y) |> Option.map string |> Option.defaultValue "."
            ]
    ]
    
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
    |> List.collect pointsCovered
    |> List.fold cover Map.empty

// sprintDiagram covered

covered
|> Map.filter (fun _ count -> count > 1)
|> Seq.length