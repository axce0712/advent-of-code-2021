open System.IO

let mostCommon pos bits =
    let ones, zeros = bits |> List.partition (fun number -> Seq.item pos number = '1')
    if ones.Length >= zeros.Length then ones else zeros
    
let leastCommon pos bits =
    let ones, zeros = bits |> List.partition (fun number -> Seq.item pos number = '1')
    if zeros.Length <= ones.Length then zeros else ones

let findRating determine candidates =
    let rec imp pos left =
        match left with
        | [ number ] -> number
        | _ -> imp (pos + 1) (determine pos left)

    imp 0 candidates

let binaryNumberToInt (input: string) =
    input
    |> Seq.rev
    |> Seq.indexed
    |> Seq.sumBy (fun (n, x) -> if x = '1' then Seq.replicate n 2 |> Seq.fold (*) 1 else 0)

let numbers =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
    |> Seq.toList

let oxygenGeneratorRating =
    findRating mostCommon numbers |> binaryNumberToInt

let co2ScrubberRating =
    findRating leastCommon numbers |> binaryNumberToInt

oxygenGeneratorRating * co2ScrubberRating
