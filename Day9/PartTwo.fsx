open System
open System.IO

let parseLine y (line : string) =
    [ for x, c in Seq.indexed line -> (x, y), int c - int '0' ]

let parse (lines : seq<string>) =
    Map.ofList [
        for y, line in Seq.indexed lines do
            for pos, n in parseLine y line do
                pos, n ]

let adjacentLocations area (x, y) =
    let xs = [x - 1..x + 1]
    let ys = [y - 1..y + 1]
    let nexts = [
        for ny in ys do
            for nx in xs do
                if (nx, ny) <> (x, y) && Map.containsKey (nx, ny) area then
                    yield (nx, ny)
    ]
    nexts

let nextLocations area (x, y) =
    let nexts = [
        yield! [
            for ny in [y - 1; y + 1] do
                if Map.containsKey (x, ny) area then
                    yield (x, ny)
        ]
        yield! [
            for nx in [x - 1; x + 1] do
                if Map.containsKey (nx, y) area then
                    yield (nx, y)
        ]
    ]
    nexts

let lowPoints area =
    [
        for (x, y), n in Map.toList area do
            let adjacents = adjacentLocations area (x, y)
            if Seq.forall (fun an -> n < Map.find an area) adjacents then
                yield (x, y)
    ]

let isBasin n = n < 9

let determineBasin area lowPoint =
    let rec imp acc point =
        let nexts =
            nextLocations area point
            |> List.filter (fun pos -> isBasin (Map.find pos area) && not (Set.contains pos acc))
            |> set

        let newAcc = Set.union acc nexts
        Set.fold imp newAcc nexts

    imp (Set.empty.Add(lowPoint)) lowPoint

// let content =
//     "2199943210
// 3987894921
// 9856789892
// 8767896789
// 9899965678".Split('\n')

let content =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
    
let area = parse content

lowPoints area
|> List.map (determineBasin area >> Set.count)
|> List.sortDescending
|> List.take 3
|> List.reduce (*)