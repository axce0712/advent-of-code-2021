open System.IO

let parse (input : string) =
    input.Split(',')
    |> Seq.map int
    |> Seq.toList

let diff from till =
    if from < till then till - from
    elif till < from then from - till
    else 0
    
let rec sumOfSteps n =
    let rec imp acc n =
        if n > 0
        then imp (acc + n) (n - 1)
        else acc

    imp 0 n

let wastedFuels till positions =
    positions |> List.sumBy (diff till >> sumOfSteps)

let cheapestOutcome positions =
    let min = List.min positions
    let max = List.max positions
    
    [min..max]
    |> Seq.map (fun till -> till, wastedFuels till positions)
    |> Seq.pairwise
    |> Seq.skipWhile (fun ((_, f1), (_, f2)) -> f1 > f2)
    |> Seq.head
    |> fst

// let content = "16,1,2,0,4,2,7,1,2,14"

let content = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

parse content |> cheapestOutcome |> snd
