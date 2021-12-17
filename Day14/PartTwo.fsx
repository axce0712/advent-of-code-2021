open System
open System.IO

let parseRule (input : string) =
    let [| target; element |] = input.Split(" -> ")
    target, element

let parse newLine (input : string) =
    let [| templatePart; rulesPart |] = input.Split(String.replicate 2 newLine)
    let rules =
        rulesPart.Split(newLine)
        |> Seq.map parseRule
        |> Map.ofSeq

    templatePart, rules

let insert rules (value : string) =
    match Map.tryFind value rules with
    | Some element -> value[..0] + element, Some (element + value[1..])
    | None _ -> value, None

let init (input : string) =
    let actual =
        input
        |> Seq.chunkBySize 2
        |> Seq.map (fun xs -> String xs, 1L)
        |> Seq.toList

    let next =
        input[1..]
        |> Seq.chunkBySize 2
        |> Seq.map (fun xs -> String xs, 1L)
        |> Seq.toList

    Map.ofList actual, Map.ofList (actual @ next)

let addCount pair count m =
    let newCount =
        Map.tryFind pair m
        |> Option.map ((+) count)
        |> Option.defaultValue count

    Map.add pair newCount m

let step rules actual =
    ((Map.empty, Map.empty), actual)
    ||> Map.fold (fun (actualSoFar, nextSoFar) pair count ->
        let newPair, nextPair = insert rules pair
        let newActual = addCount newPair count actualSoFar
        let newNext =
            nextPair
            |> Option.map (fun p -> addCount p count nextSoFar)
            |> Option.defaultValue nextSoFar
            |> addCount newPair count
        newActual, newNext)

let count actual =
    actual
    |> Map.fold (fun acc pair count ->
        pair
        |> Seq.map (fun c -> c, count)
        |> Seq.fold (fun innerAcc (c, cn) -> addCount c cn innerAcc) acc) Map.empty

let solve rules template =
    let elements =
        (snd >> (step rules))
        |> Seq.replicate 40 
        |> Seq.fold (|>) (init template)
        |> fst
        |> count
        |> Map.toList
        
    let mostCommon = List.maxBy snd elements |> snd
    let leastCommon = List.minBy snd elements |> snd
    mostCommon - leastCommon

let content =
    "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"

let template, rules = parse "\n" content

// let content =
//     File.ReadAllText (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))

// let template, rules = parse Environment.NewLine content

solve rules template