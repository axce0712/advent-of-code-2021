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
    let element = Map.find value rules
    value[..0] + element, element + value[1..]

let addCount pair count m =
    let newCount =
        Map.tryFind pair m
        |> Option.map ((+) count)
        |> Option.defaultValue count

    Map.add pair newCount m

let init (input : string) =
    let actual =
        input
        |> Seq.chunkBySize 2
        |> Seq.fold (fun acc xs -> addCount (String xs) 1L acc) Map.empty

    let next =
        input
        |> Seq.windowed 2
        |> Seq.fold (fun acc xs -> addCount (String xs) 1L acc) Map.empty

    actual, next

let step rules actual =
    actual
    |> Map.fold (fun (actualSoFar, nextSoFar) pair count ->
        let newPair, nextPair = insert rules pair
        let newActual = addCount newPair count actualSoFar
        let newNext =
            nextSoFar
            |> addCount newPair count
            |> addCount nextPair count
            
        newActual, newNext) (Map.empty, Map.empty)

let count actual =
    actual
    |> Map.toSeq
    |> Seq.collect (fun (pair, count) -> pair |> Seq.map (fun c -> c, count))
    |> Seq.fold (fun acc (c, cnt) -> addCount c cnt acc) Map.empty

let solve iterationCount rules template =
    let chart =
        (snd >> (step rules))
        |> Seq.replicate iterationCount 
        |> Seq.fold (|>) (init template)
        |> fst
        |> addCount (template[^0..]) 1L
        |> count
        |> Map.toList
        
    let mostCommon = chart |> List.map snd |> List.max
    let leastCommon = chart |> List.map snd |> List.min
    mostCommon - leastCommon

// let content =
//    "NNCB

// CH -> B
// HH -> N
// CB -> H
// NH -> C
// HB -> C
// HC -> B
// HN -> C
// NN -> C
// BH -> H
// NC -> B
// NB -> B
// BN -> B
// BB -> N
// BC -> B
// CC -> N
// CN -> C"

// let template, rules = parse "\n" content

let content =
    File.ReadAllText (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))

let template, rules = parse Environment.NewLine content
solve 40 rules template
