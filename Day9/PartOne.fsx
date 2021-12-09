open System
open System.IO

let parseLine y (line : string) =
    [ for x, c in Seq.indexed line -> (x, y), int c - int '0' ]

let parse (lines : seq<string>) =
    Map.ofList [
        for y, line in Seq.indexed lines do
            for kvp in parseLine y line do
                kvp ]

let adjacentLocations area (x, y) =
    let xs = [x - 1..x + 1]
    let ys = [y - 1..y + 1]
    let nexts = [
        for ny in ys do
            for nx in xs do
                if (nx, ny) <> (x, y) then
                    match Map.tryFind (nx, ny) area with
                    | Some n -> yield n
                    | None -> ()
    ]
    nexts

let lowPoints area =
    [
        for (x, y), n in Map.toList area do
            let adjacents = adjacentLocations area (x, y)
            if Seq.forall (fun an -> n < an) adjacents then
                yield (x, y), n
    ]

let riskLevel lowPoints =
    lowPoints
    |> List.sumBy (fun (_, n) -> n + 1)

// let content =
//     "2199943210
// 3987894921
// 9856789892
// 8767896789
// 9899965678".Split('\n')

let content =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
let area =
    parse (content)

lowPoints area |> riskLevel