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

let toInt bits =
    (1, 0)
    |> List.foldBack (fun c (value, acc) ->
        let newAcc =
            if c = 1 then value + acc else acc
        (value * 2, newAcc)) bits
    |> snd

let version bits =
    let versionBits, remaining = List.splitAt 3 bits
    toInt versionBits, remaining 

let processLiteralValue bits =
    let rec imp acc bits =
        let literalBits, remaining = List.splitAt 5 bits
        match literalBits with
        | 1 :: numberBits -> imp (acc @ numberBits) remaining
        | 0 :: numberBits -> toInt (acc @ numberBits), remaining
        | _ -> invalidOp (sprintf "%A" bits)

    imp [] bits

let (|Version|) packet = version packet

let (|LiteralValue|Operation|) bits =
    match bits with
    | 1 :: 0 :: 0 :: remaining -> LiteralValue remaining
    | _ :: _ :: _ :: remaining -> Operation remaining
    | _ -> failwith "invalid value"

let rec step bits =
    match bits with
    | xs when List.forall ((=) 0) xs -> [], []
    | Version (version, LiteralValue remaining) ->
        let value, remaining = processLiteralValue remaining
        [ version ], remaining
    | Version (version, Operation (0 :: remaining)) ->
        let lengthBits, remaining = List.splitAt 15 remaining
        let length = toInt lengthBits
        let subPacketBits, remaining = List.splitAt length remaining
        let versions =
            subPacketBits
            |> List.unfold (fun remaining ->
                match remaining with
                | [] -> None
                | remaining -> Some (step remaining))

        version :: List.collect id versions, remaining
    | Version (version, Operation (1 :: remaining)) ->
        let countBits, remaining = List.splitAt 11 remaining
        let count = toInt countBits
        let versionss, finalRemaining =
            [1..3]
            |> List.mapFold (fun remaining _ ->
                let versions, newRemaining = step remaining
                versions, newRemaining) remaining

        version :: (List.collect id versionss), finalRemaining
    | _ -> failwith ""

let solve input =
    let rec imp bits =
        let versions, remaining = step bits
        match remaining with
        | [] -> versions
        | rs when List.forall ((=) 0) rs -> versions
        | rs -> versions @ (imp rs)

    imp (decode input) |> List.sum

// decode "D2FE28" |> step 
// decode "38006F45291200" |> step 
// decode "EE00D40C823060" |> step 
// solve "8A004A801A8002F478"
// solve "620080001611562C8802118E34"
// solve "C0015000016115A2E0802F182340"
// solve "A0016C880162017C3686B18A3D4780"

let content =
    File.ReadAllText (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))

solve content