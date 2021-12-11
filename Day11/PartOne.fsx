open System

type State =
    | EnergyLevel of int
    | Flashing
    | Flashed

let parse (content : string) =
    Map.ofList [
        for y, line in content.Split('\n') |> Seq.indexed do
            for x, c in Seq.indexed line do
                (x, y), EnergyLevel (int c - int '0')
    ]

let adjecant (x, y) = [
    for dx in [ -1; 1 ] do
        for dy in [ -1 .. 1 ] do
            x + dx, y + dy
    for dy in [ -1; 1 ] do
        x, y + dy
]

let tryIncrement = function
    | EnergyLevel 9 -> Some Flashing
    | EnergyLevel n -> Some (EnergyLevel (n + 1))
    | _ -> None

let increment =
    Map.map (fun _ s -> tryIncrement s |> Option.defaultValue s)
    
let rec applyFlash m =
    let newM =
        (m, Map.keys m)
        ||> Seq.fold (fun acc pos ->
            match Map.find pos acc with
            | Flashing ->
                let adjecant =
                    adjecant pos
                    |> List.filter (fun p -> Map.containsKey p acc)
                
                (acc, adjecant)
                ||> List.fold (fun a p ->
                    Map.tryFind p a
                    |> Option.bind tryIncrement
                    |> Option.map (fun s -> Map.add p s a)
                    |> Option.defaultValue a)
                |> Map.add pos Flashed
            | _ -> acc)

    if Map.exists (fun _ -> function Flashing -> true | _ -> false) newM then
        applyFlash newM
    else
        newM
    
let normalize m =
    Map.map (fun _ -> function Flashed -> EnergyLevel 0 | state -> state) m

let step = 
    increment >> applyFlash >> normalize

let countZeros m =
    m
    |> Map.toSeq
    |> Seq.sumBy (fun (_, state) -> match state with | EnergyLevel 0 -> 1 | _ -> 0)

let print (m : Map<int * int, State>) =
    let allPositions = Map.keys m
    let minX = allPositions |> Seq.map fst |> Seq.min
    let minY = allPositions |> Seq.map snd |> Seq.min
    let maxX = allPositions |> Seq.map fst |> Seq.max
    let maxY = allPositions |> Seq.map snd |> Seq.max
    String.concat "\n" [
        for y in [minY..maxY] do
            String.concat "" [
                for x in [minX..maxX] do
                    match Map.find (x, y) m with
                    | EnergyLevel n -> string n
                    | Flashing -> "0"
                    | Flashed -> "0"
            ]
    ]

let content =
    "3265255276
1537412665
7335746422
6426325658
3854434364
8717377486
4522286326
6337772845
8824387665
6351586484"

[0..100]
|> List.mapFold (fun acc _ -> countZeros acc, step acc) (parse content)
|> fst
|> Seq.sum