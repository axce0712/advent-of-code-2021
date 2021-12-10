open System.IO

type ParseResult =
    | Incomplete of Remaining : char list
    | Corrupted
    | Valid

let (|Open|Close|) chunk =
    match chunk with
    | '(' | '[' | '{' | '<' -> Open chunk
    | ')' | ']' | '}' | '>' -> Close chunk
    | _ -> failwith $"unexpected chunk '{chunk}'"

let invert chunk =
    match chunk with
    | '(' -> ')'
    | ')' -> '('
    | '[' -> ']'
    | ']' -> '['
    | '{' -> '}'
    | '}' -> '{'
    | '<' -> '>'
    | '>' -> '<'
    | _ -> failwith $"unexpected chunk '{chunk}'"

let parseChunk line =
    let rec imp opened remaining =
        match remaining, opened with
        | [], [] -> Valid
        | [], _  -> Incomplete opened
        | Open  x :: xs, _ -> imp (x :: opened) xs
        | Close x :: xs, y :: ys when x = invert y -> imp ys xs
        | Close x :: _ , _ -> Corrupted

    imp [] line

let score =
    function
    | ')' -> 1L
    | ']' -> 2L
    | '}' -> 3L
    | '>' -> 4L

let chooseIncompleted =
    function
    | Incomplete remaining -> Some remaining
    | _ -> None

let determineScore =
    List.fold (fun scoreSoFar chunk -> scoreSoFar * 5L + score (invert chunk)) 0L

// let content =
//     "[({(<(())[]>[[{[]{<()<>>
// [(()[<>])]({[<{<<[]>>(
// {([(<{}[<>[]}>{[]{[(<()>
// (((({<>}<{<{<>}{[]{[]{}
// [[<[([]))<([[{}[[()]]]
// [{[{({}]{}}([{[{{{}}([]
// {<[[]]>}<{[{[{[]{()[[[]
// [<(<(<(<{}))><([]([]()
// <{([([[(<>()){}]>(<<{{
// <{([{{}}[<[[[<>{}]]]>[]]".Split('\n')

let content =
    File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

content
|> Seq.choose (Seq.toList >> parseChunk >> chooseIncompleted)
|> Seq.map determineScore
|> Seq.sort
|> Seq.toList
|> fun xs -> xs.[xs.Length / 2]