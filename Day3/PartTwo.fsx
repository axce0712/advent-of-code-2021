open System.IO

let numbers =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
    |> Seq.toList

let findOxygenGeneratorRating candidates =
    let rec imp pos left =
        match left with
        | [ number ] -> number
        | _ ->
            let ones, zeros = left |> List.partition (fun number -> Seq.item pos number = '1')
            let newLeft = if ones.Length >= zeros.Length then ones else zeros
            imp (pos + 1) newLeft

    imp 0 candidates

let findCo2ScrubberRating candidates =
    let rec imp pos left =
        match left with
        | [ number ] -> number
        | _ ->
            let ones, zeros = left |> List.partition (fun number -> Seq.item pos number = '1')
            let newLeft = if zeros.Length <= ones.Length then zeros else ones
            imp (pos + 1) newLeft

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
