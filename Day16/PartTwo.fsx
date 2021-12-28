open System
open System.IO

let toBinary = function
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'A' -> "1010"
    | 'B' -> "1011"
    | 'C' -> "1100"
    | 'D' -> "1101"
    | 'E' -> "1110"
    | 'F' -> "1111"

let decode (input : string) =
    input
    |> Seq.collect toBinary
    |> Seq.map (fun c -> int c - int '0')
    |> Seq.toList

let toInt64 bits =
    (1L, 0L)
    |> List.foldBack (fun c (value, acc) ->
        let newAcc =
            if c = 1 then value + acc else acc
        (value * 2L, newAcc)) bits
    |> snd

let version bits =
    let versionBits, remaining = List.splitAt 3 bits
    toInt64 versionBits, remaining 

let processLiteralValue bits =
    let rec imp acc bits =
        let literalBits, remaining = List.splitAt 5 bits
        match literalBits with
        | 1 :: numberBits -> imp (acc @ numberBits) remaining
        | 0 :: numberBits -> toInt64 (acc @ numberBits), remaining
        | _ -> invalidOp (sprintf "%A" bits)

    imp [] bits

let (|Version|) packet = version packet

let rec step bits =
    match bits with
    | xs when List.forall ((=) 0) xs -> [], []
    | Version (_, 0 :: 0 :: 0 :: remaining) -> sum remaining
    | Version (_, 0 :: 0 :: 1 :: remaining) -> product remaining
    | Version (_, 0 :: 1 :: 0 :: remaining) -> minimum remaining
    | Version (_, 0 :: 1 :: 1 :: remaining) -> maximum remaining      
    | Version (_, 1 :: 0 :: 0 :: remaining) -> literalValue remaining
    | Version (_, 1 :: 0 :: 1 :: remaining) -> greaterThan remaining
    | Version (_, 1 :: 1 :: 0 :: remaining) -> lessThan remaining
    | Version (_, 1 :: 1 :: 1 :: remaining) -> equalTo remaining
and subPackets bits =
    match bits with
    | 0 :: remaining ->
        let lengthBits, remaining = List.splitAt 15 remaining
        let length = toInt64 lengthBits
        let subPacketBits, remaining = List.splitAt (int length) remaining
        let values =
            subPacketBits
            |> List.unfold (fun remaining ->
                match remaining with
                | [] -> None
                | remaining -> Some (step remaining))

        List.collect id values, remaining
    | 1 :: remaining ->
        let countBits, remaining = List.splitAt 11 remaining
        let count = toInt64 countBits
        let xss, finalRemaining =
            [1L..count]
            |> List.mapFold (fun remaining _ -> step remaining) remaining

        (List.collect id xss), finalRemaining
and sum bits =
    let values, remaining = subPackets bits
    [ List.sum values ], remaining
and product bits =
    let values, remaining = subPackets bits
    [ List.reduce (*) values ], remaining
and minimum bits =
    let values, remaining = subPackets bits
    [ List.min values ], remaining
and maximum bits =
    let values, remaining = subPackets bits
    [ List.max values ], remaining
and literalValue bits =
    let value, remaining = processLiteralValue bits
    [ value ], remaining
and greaterThan bits =
    let [ x; y ], remaining = subPackets bits
    let result = if x > y then 1 else 0
    [ result ], remaining
and lessThan bits =
    let [ x; y ], remaining = subPackets bits
    let result = if x < y then 1 else 0
    [ result ], remaining
and equalTo bits =
    let [ x; y ], remaining = subPackets bits
    let result = if x = y then 1 else 0
    [ result ], remaining

let solve input =
    decode input
    |> step
    |> fst
    |> List.head

// decode "C200B40A82" |> step
// decode "04005AC33890" |> step
// decode "880086C3E88112" |> step
// decode "CE00C43D881120" |> step
// decode "D8005AC2A8F0" |> step
// decode "F600BC2D8F" |> step
// decode "9C005AC2F8F0" |> step
// decode "9C0141080250320F1802104A08" |> step

let content =
    File.ReadAllText (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))

solve content