open System
open System.IO

type ParseResult =
    | Incomplete
    | Corrupted of char * int
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

let tryFindInvalidChunk line =
    let rec imp pos opened remaining =
        match remaining, opened with
        | [], [] -> Valid
        | [], _  -> Incomplete
        | Open  x :: xs, _ -> imp (pos + 1) (x :: opened) xs
        | Close x :: xs, y :: ys when x = invert y -> imp (pos + 1) ys xs
        | Close x :: _ , _ -> Corrupted (x, pos)

    imp 0 [] line

let score = function
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137

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

// content
// |> Seq.map (Seq.toList >> tryFindInvalidChunk)
// |> Seq.sumBy (function Corrupted (illegal, _) -> score illegal | _ -> 0)

File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
|> Seq.map (Seq.toList >> tryFindInvalidChunk)
|> Seq.sumBy (function Corrupted (illegal, _) -> score illegal | _ -> 0)