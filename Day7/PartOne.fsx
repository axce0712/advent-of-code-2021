open System
open System.IO

let parse (input : string) =
    input.Split(',')
    |> Seq.map int
    |> Seq.toList

let move from till =
    let rec imp fuel pos =
        if pos < till then imp (fuel + 1) (pos + 1)
        elif till < pos then imp (fuel + 1) (pos - 1)
        else fuel

    imp 0 from

let wastedFuels till positions =
    positions |> List.sumBy (fun p -> move p till)



let content = "16,1,2,0,4,2,7,1,2,14"

parse content

// File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
