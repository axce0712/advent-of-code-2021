open System
open System.IO

type Position = { X : int; Y : int }

type Cell =
    | Number of int
    | Marked

type Board = Board of Map<Position, Cell>

type Bingo =
    { Numbers : int list
      Boards : Board list }

let parseBoard (newLine : string) (content : string) =
    let lines = content.Split([| newLine |], StringSplitOptions.RemoveEmptyEntries)
    let board =
        Map.ofList [
            for y, line in Seq.indexed lines do
                let columns = line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
                for x, number in Seq.indexed columns do
                    let position = { X = x; Y = y }
                    let cell = int number |> Number
                    position, cell
        ]
        |> Board

    board

let parse newLine (content : string) =
    let separator = String.replicate 2 newLine
    let parts = content.Split([| separator |], StringSplitOptions.RemoveEmptyEntries)
    let numbers =
        parts[0].Split(',') |> Array.map int |> Array.toList

    let boards =
        parts[1..]
        |> Array.map (parseBoard newLine)
        |> Array.toList

    { Numbers = numbers; Boards = boards }

let mark number (Board m) =
    m
    |> Map.map (fun _ cell ->
        match cell with
        | Number n when number = n -> Marked
        | _ -> cell)
    |> Board

let hasMarkedRow (Board m) =
    Map.toSeq m
    |> Seq.groupBy (fun (pos, _) -> pos.Y)
    |> Seq.exists (fun (_, cells) -> cells |> Seq.map snd |> Seq.forall (function Marked -> true | _ -> false))

let hasMarkedColumn (Board m) =
    Map.toSeq m
    |> Seq.groupBy (fun (pos, _) -> pos.X)
    |> Seq.exists (fun (_, cells) -> cells |> Seq.map snd |> Seq.forall (function Marked -> true | _ -> false))

let sumOfNumbers (Board m) =
    Map.toSeq m
    |> Seq.sumBy (fun (_, cell) -> match cell with | Number n -> n | _ -> 0)

let tryFindWinningBoard bingo =
    let rec imp boardsSoFar numbersSoFar =
        match numbersSoFar with
        | n :: ns ->
            let newBoards = List.map (mark n) boardsSoFar
            let winningBoard =
                newBoards
                |> List.tryFind (fun b -> hasMarkedRow b || hasMarkedColumn b)

            match winningBoard with
            | Some board -> Some (n, board)
            | None -> imp newBoards ns
        | _ -> None
    
    imp bingo.Boards bingo.Numbers

// let content =
//     "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

// 22 13 17 11  0
//  8  2 23  4 24
// 21  9 14 16  7
//  6 10  3 18  5
//  1 12 20 15 19

//  3 15  0  2 22
//  9 18 13 17  5
// 19  8  7 25 23
// 20 11 10 24  4
// 14 21 16 12  6

// 14 21 17 24  4
// 10 16 15  9 19
// 18  8 23 26 20
// 22 11 13  6  5
//  2  0 12  3  7"

// let bingo =
//     parse "\n" content

let bingo =
    File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
    |> parse Environment.NewLine 

tryFindWinningBoard bingo
|> Option.map (fun (lastNumber, board) -> lastNumber * sumOfNumbers board)
