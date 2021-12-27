open System
open System.IO

let parseRule (input : string) =
    let [| target; element |] = input.Split(" -> ")
    Seq.toList target, Seq.head element

let parse newLine (input : string) =
    let [| templatePart; rulesPart |] = input.Split(String.replicate 2 newLine)
    let rules =
        rulesPart.Split(newLine)
        |> Seq.map parseRule
        |> Map.ofSeq

    Seq.toList templatePart, rules

let apply rules pair =
    let [ a; _ ] = pair
    let e = Map.find pair rules
    [ a; e ]

let step rules list =
    let mapped = 
        list
        |> List.windowed 2
        |> List.collect (apply rules)

    mapped @ list[^0..]

let solve iterationCount rules template =
    let occurrences =
        Seq.replicate iterationCount (step rules)
        |> Seq.fold (|>) template
        |> Seq.countBy id
        |> Seq.toList

    let leastCommon = occurrences |> List.minBy snd |> snd
    let mostCommon = occurrences |> List.maxBy snd |> snd
    mostCommon - leastCommon

// let content =
//     "NNCB

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
solve 10 rules template
