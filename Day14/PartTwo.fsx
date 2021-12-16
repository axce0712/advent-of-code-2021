open System

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

let init (template : string) =
    let current =
        template
        |> Seq.chunkBySize 2
        |> Seq.map (fun xs -> String xs, 1L)
        |> Map

    let nexts =
        template
        |> Seq.windowed 2
        |> Seq.map (fun xs -> String xs, 1L)
        |> Map
        |> Map.add template[^0..] 1
    current, nexts

let insert rules (pair : string) =
    match Map.tryFind pair rules with
    | Some element -> pair[..0] + element, Some (element + pair[1..])
    | None -> pair, None

let addCount pair count m =
    let newCount =
        m
        |> Map.tryFind pair
        |> Option.map ((+) count)
        |> Option.defaultValue count

    Map.add pair newCount m

let step rules =
    (Map.empty, Map.empty)
    |> Map.fold (fun (currentSoFar, nextSoFar) pair count ->
        let newPair, nextPair = insert rules pair
        let newCurrent = addCount newPair count currentSoFar
        let newNext =
            nextPair
            |> Option.map (fun p -> addCount p count nextSoFar)
            |> Option.defaultValue nextSoFar
            |> addCount newPair count
            
        newCurrent, newNext)

let count m =
    (Map.empty, m)
    ||> Map.fold (fun acc pair count ->
        pair
        |> Seq.map (fun p -> p, count)
        |> Seq.fold (fun inner (p, c) -> addCount p c inner) acc)
    
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
let result, _ = 
    (snd >> step rules)
    |> Seq.replicate 40
    |> Seq.fold (|>) (init template)

let counted = Map.toList (count result)
let mostCommon = counted |> Seq.map snd |> Seq.max
let leastCommon = counted |> Seq.map snd |> Seq.min
mostCommon - leastCommon

(*
NNCB

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
CN -> C

Template:     NNCB
After step 1: NCNBCHB
After step 2: NBCCNBBBCBHCB
After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
*)