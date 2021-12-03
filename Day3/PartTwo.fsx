open System.IO

let numbers =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
    |> Seq.toList

let findOxygenGeneratorRating candidates =
    let rec imp pos left =
        match left with
        | [ number ] -> number
        | _ ->
            let grouped =
                left
                |> List.groupBy (fun number -> Seq.item pos number)

            let (ones :: _), (zeros :: _) =
                List.partition (fun (bit, _) -> bit = '1') grouped

            match List.length (snd ones), List.length (snd zeros) with
            | onesCount, zeroCounts when onesCount = zeroCounts -> imp (pos + 1) (snd ones)
            | onesCount, zeroCounts when onesCount > zeroCounts -> imp (pos + 1) (snd ones)
            | onesCount, zeroCounts when onesCount < zeroCounts -> imp (pos + 1) (snd zeros)

    imp 0 candidates

let findCo2ScrubberRating candidates =
    let rec imp pos left =
        match left with
        | [ number ] -> number
        | _ ->
            let grouped =
                left
                |> List.groupBy (fun number -> Seq.item pos number)

            let (ones :: _), (zeros :: _) =
                List.partition (fun (bit, _) -> bit = '1') grouped

            match List.length (snd ones), List.length (snd zeros) with
            | onesCount, zeroCounts when onesCount = zeroCounts -> imp (pos + 1) (snd zeros)
            | onesCount, zeroCounts when onesCount > zeroCounts -> imp (pos + 1) (snd zeros)
            | onesCount, zeroCounts when onesCount < zeroCounts -> imp (pos + 1) (snd ones)

    imp 0 candidates

let binaryNumberToInt (input: string) =
    input
    |> Seq.rev
    |> Seq.indexed
    |> Seq.sumBy (fun (n, x) ->
        if x = '1'
        then Seq.replicate n 2 |> Seq.fold (fun a b -> a * b) 1
        else 0)

let oxygenGeneratorRating =
    findOxygenGeneratorRating numbers |> binaryNumberToInt

let co2ScrubberRating =
    findCo2ScrubberRating numbers |> binaryNumberToInt

oxygenGeneratorRating * co2ScrubberRating
