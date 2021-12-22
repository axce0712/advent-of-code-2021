open System
open System.IO

type Range = { From : int; To : int }

type Instruction =
    | On of x : Range * y : Range * z : Range
    | Off of x : Range * y : Range * z : Range

let isBetween { From = f1; To = t1 } { From = f2; To = t2 } =
    f1 <= f2 && t2 <= t1

let parseRange (input : string) =
    let [| fromPart; toPart |] = input.Split ("..", StringSplitOptions.RemoveEmptyEntries)
    { From = int fromPart; To = int toPart }

let parseCuboid (input : string) =
    let [| xPart; yPart; zPart |] = input.Split ','
    let x = parseRange xPart[2..]
    let y = parseRange yPart[2..]
    let z = parseRange zPart[2..]
    x, y, z

let parseStep (input : string) =
    match input.Split ' ' with
    | [| "on"; cuboidPart |] -> On (parseCuboid cuboidPart)
    | [| "off"; cuboidPart |] -> Off (parseCuboid cuboidPart)
    | _ -> invalidArg (nameof input) input

let coordinates range =
    let mutable current = range.From
    seq {
        yield current
        while current <> range.To do
            current <- if current < range.To then current + 1 else current - 1
            yield current
    }

let steps (xRange, yRange, zRange) =
    seq {
        for x in coordinates xRange do
            for y in coordinates yRange do
                for z in coordinates zRange do
                    yield (x, y, z)
    }

let step cubes = function
    | On (xRange, yRange, zRange) ->
        (cubes, steps (xRange, yRange, zRange))
        ||> Seq.fold (fun acc (x, y, z) -> Map.add (x, y, z) true acc)

    | Off (xRange, yRange, zRange) ->
        (cubes, steps (xRange, yRange, zRange))
        ||> Seq.fold (fun acc (x, y, z) -> Map.add (x, y, z) false acc)

let solve instructions =
    instructions
    |> Seq.filter (fun instruction ->
        let ranges =
            match instruction with
            | On (xRange, yRange, zRange) -> [ xRange; yRange; zRange ]
            | Off (xRange, yRange, zRange) -> [ xRange; yRange; zRange ]
            
        Seq.forall (isBetween { From = -50; To = 50 }) ranges)
    |> Seq.fold step Map.empty
    |> Map.filter (fun _ v -> v)
    |> Map.count

let example =
    "on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682"

// solve (Seq.map parseStep (example.Split '\n' ))

File.ReadLines (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))
|> Seq.map parseStep 
|> solve
