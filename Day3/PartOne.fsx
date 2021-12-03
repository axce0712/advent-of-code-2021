open System
open System.IO

let mostCommonBit bits =
    let oneCounts, zeroCounts =
        ((0, 0), bits)
        ||> List.fold (fun (ones, zeros) -> function
            | '1' -> ones + 1, zeros
            | _ -> ones, zeros + 1)

    if oneCounts > zeroCounts then '1' else '0'

let leastCommonBit bits =
    let oneCounts, zeroCounts =
        ((0, 0), bits)
        ||> List.fold (fun (ones, zeros) -> function
            | '1' -> ones + 1, zeros
            | _ -> ones, zeros + 1)

    if zeroCounts < oneCounts then '0' else '1'

let calculateBinaryNumber determineBit numbers =
    numbers
    |> List.transpose  
    |> List.map determineBit
    |> List.toArray
    |> String

let binaryNumberToInt (input : string) =
    input
    |> Seq.rev
    |> Seq.indexed
    |> Seq.sumBy (fun (n, x) ->
        if x = '1'
        then Seq.replicate n 2 |> Seq.fold (*) 1
        else 0)

let numbers =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
    |> Seq.map (fun str -> Array.toList (str.ToCharArray()))
    |> Seq.toList

let gamma =
    numbers
    |> calculateBinaryNumber mostCommonBit
    |> binaryNumberToInt

let epsilon =
    numbers
    |> calculateBinaryNumber leastCommonBit
    |> binaryNumberToInt

gamma * epsilon