open System
open System.IO

type OptionBuilder() =
    member _.Bind(x, f) = Option.bind f x
    member _.Map(x, f) = Option.map f x
    member _.Return(x) = Some x
    member _.ReturnFrom(x) = x

let option = OptionBuilder()

let tryFindZero digit m =
    option {
        let! s9 = Map.tryFindKey (fun _ n -> n = 9) m
        let! s3 = Map.tryFindKey (fun _ n -> n = 3) m
        let s4 = Map.findKey (fun _ n -> n = 4) m
        let s8 = Map.findKey (fun _ n -> n = 8) m
        let s1 = Map.findKey (fun _ n -> n = 1) m
        let b = s9 - s3 
        let d = s4 - s1 - b
        let! result =
            if (s8 - d) = digit then Some 0 else None
        return result
    }

let tryFindTwo m =
    if Map.exists (fun _ n -> n = 3) m && Map.exists (fun _ n -> n = 5) m
    then Some 2
    else None

let tryFindThree digit m =
    option {
        let! s9 = Map.tryFindKey (fun _ n -> n = 9) m
        let! s1 = Map.tryFindKey (fun _ n -> n = 1) m
        let diff = Set.difference s9 digit
        let! result =
            match Set.isProperSubset s1 digit, Set.toList diff with
            | true, [ _ ] -> Some 3
            | _ -> None
        return result
    }

let tryFindFive digit m =
    option {
        let! s6 = Map.tryFindKey (fun _ n -> n = 6) m
        let diff = Set.difference s6 digit
        let! result =
            match Seq.toList diff with
            | [ _ ] -> Some 5
            | _ -> None
        return result
    }

let tryFindSix m =
    if Map.exists (fun _ n -> n = 0) m && Map.exists (fun _ n -> n = 9) m
    then Some 6
    else None

let tryFindNine digit m =
    option {
        let! s7 = Map.tryFindKey (fun _ n -> n = 7) m
        let! s4 = Map.tryFindKey (fun _ n -> n = 4) m
        let combined = Set.union s7 s4
        let diff = Set.difference digit combined
        let! result =
            match Set.toList diff with
            | [ _ ] -> Some 9
            | _     -> None
        return result
    }

let findMatch (m : Map<Set<char>, int>) (digit : Set<char>) =
    match Map.tryFind digit m with
    | Some n -> Some n
    | None ->
        match digit.Count with
        | 2 -> Some 1 // 1
        | 3 -> Some 7 // 7
        | 4 -> Some 4 // 4
        | 5 -> // 2 3 5
            tryFindThree digit m
            |> Option.orElse (tryFindFive digit m)
            |> Option.orElse (tryFindTwo m)
        | 6 -> // 0 6 9
            tryFindNine digit m
            |> Option.orElse (tryFindZero digit m)
            |> Option.orElse (tryFindSix m)
        | 7 -> Some 8 // 8
        | _ -> None

let run m digits =
    let rec imp unresolved m ds =
        match ds with
        | [] -> m, List.rev unresolved
        | d :: ds ->
            match findMatch m d with
            | None -> imp (d :: unresolved) m ds
            | Some n ->  imp unresolved (Map.add d n m) ds

    imp [] m digits

let resolveNumbers digits =
    let rec imp acc left =
        match left with
        | [] -> acc
        | ls -> run acc ls ||> imp

    imp Map.empty digits

let resolveOutput (line : string) =
    let parts =
        line.Split("|")
        |> Seq.map (fun part -> part.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.map set |> Seq.toList)
        |> Seq.toList

    let digits = parts |> List.collect id
    let numbers = resolveNumbers digits
    let output = parts.[1]
    let asNumber =
        output
        |> Seq.map (fun s -> Map.find s numbers |> string)
        |> String.concat ""
    asNumber

// let line = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

// line.Split([| " "; "|" |], StringSplitOptions.RemoveEmptyEntries)
// |> Seq.map set
// |> Seq.toList
// |> resolveNumbers

// let content =
//     "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
// edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
// fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
// fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
// aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
// fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
// dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
// bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
// egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
// gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce".Split('\n')

// content
// |> Seq.sumBy (resolveOutput >> int)

File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
|> Seq.sumBy (resolveOutput >> int)