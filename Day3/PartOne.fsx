open System
open System.IO

let numbers =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
    |> Seq.map (fun str -> Array.toList (str.ToCharArray()))
    |> Seq.toList
    |> List.transpose

let gammaBinaryNumber =
    numbers
    |> List.map (fun xs ->
        let ones = xs |> List.filter ((=) '1') |> List.length
        let zeros = xs.Length - ones
        if ones > zeros then "1" else "0")
    |> String.concat ""

let epsilonBinaryNumber = String.map (function '1' -> '0' | _ -> '1') gammaBinaryNumber

let binaryNumberToInt (input : string) =
    input
    |> Seq.rev
    |> Seq.indexed
    |> Seq.sumBy (fun (n, x) ->
        if x = '1'
        then Seq.replicate n 2 |> Seq.fold (fun a b -> a * b) 1
        else 0)

binaryNumberToInt gammaBinaryNumber * binaryNumberToInt epsilonBinaryNumber