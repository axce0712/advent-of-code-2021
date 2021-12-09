open System
open System.IO

type OptionBuilder() =
    member _.Bind(x, f) = Option.bind f x
    member _.Map(x, f) = Option.map f x
    member _.Return(x) = Some x
    member _.ReturnFrom(x) = x

let option = OptionBuilder()

let parse (line : string) =
    line.Split('|')
        .[1]
        .Split(' ', StringSplitOptions.RemoveEmptyEntries)
    |> Seq.toList

let tryFindThree digit m =
    option {
        let! s9s = Map.tryFindKey (fun _ n -> n = 9) m
        let! s1s = Map.tryFindKey (fun _ n -> n = 1) m
        let diff = Set.difference s9s digit
        return!
            match Set.isProperSubset s1s digit, Set.toList diff with
            | true, [ _ ] -> Some 3
            | _ -> None
    }

let tryFindNine digit m =
    option {
        let! s7s = Map.tryFindKey (fun _ n -> n = 7) m
        let! s4s = Map.tryFindKey (fun _ n -> n = 4) m
        let combined = Set.union s7s s4s
        let diff = Set.difference digit combined
        return!
            match Set.toList diff with
            | [ _ ] -> Some 9
            | _     -> None
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
        | 6 -> // 0 6 9
            tryFindNine digit m
        | 7 -> Some 8 // 8
        | _ -> None

let resolveSegments m =
    option {
        let! s7s = Map.tryFindKey (fun _ n -> n = 7) m
        let! s1s = Map.tryFindKey (fun _ n -> n = 1) m
        return!
            match Set.difference s7s s1s |> Seq.toList with
            | [ s ] -> Some ("a", string s)
            | _ -> None
    }

let content =
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce".Split('\n')

let line = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

let digits (line : string) =
    line.Split([| " "; "|" |], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map set
    |> Seq.toList

let run m digits =
    let rec imp unresolved m ds =
        match ds with
        | [] -> m, List.rev unresolved
        | d :: ds ->
            match findMatch m d with
            | None -> imp (d :: unresolved) m ds
            | Some n ->  imp unresolved (Map.add d n m) ds

    imp [] m digits

(Map.empty, (digits line))
||> run 
||> run
||> run

// let segments = resolveSegments matches