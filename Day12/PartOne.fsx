open System

type Position =
    | Start
    | End
    | Small of string
    | Big of string

type Path = { From : Position; To : Position }

type PathResult =
    {
        Current : Position
        Remaining : Map<Position, Position list>
        Visited : Position list
    }

let parsePosition (input : string) =
    match input with
    | "start" -> Start
    | "end" -> End
    | input when Seq.forall Char.IsUpper input -> Big input
    | input -> Small input

let parsePath (input : string) =
    let parts = input.Split '-'
    let [| f; t |] = parts |> Array.map parsePosition
    { From = f; To = t }

let parse (content : string) =
    content.Split '\n'
    |> Seq.map parsePath
    |> Seq.toList

let invert { From = f; To = t } = { From = t; To = f }

let printPosition = function
    | Start -> "start"
    | End -> "end"
    | Small s -> s
    | Big l -> l

let printPath { From = f; To = t } =
    sprintf "%s-%s" (printPosition f) (printPosition t)

fsi.AddPrinter<Path>(printPath)

let possbileCombinations paths =
    paths
    |> List.collect (fun p -> [ p; invert p ])
    |> List.groupBy (fun p -> p.From)
    |> List.map (fun (from, ps) -> from, ps |> List.map (fun p -> p.To))
    |> Map.ofList
    |> Map.remove End

let init combinations =
    let nexts =
        Map.find Start combinations

    let remaining =
        combinations
        |> Map.remove Start
        |> Map.map (fun _ ps -> ps |> List.filter ((<>) Start))

    nexts
    |> List.map (fun p -> { Current = p; Visited = [ Start ]; Remaining = remaining })

let rec findPath { Current = current; Visited = visited; Remaining = remaining } =
    match Map.tryFind current remaining with
    | None -> [ current :: visited ]
    | Some nexts ->
        let newRemaining =
            match current with
            | Small _ ->
                remaining
                |> Map.remove current
                |> Map.map (fun _ ps -> ps |> List.filter ((<>) current))
            | _ -> remaining

        nexts
        |> List.collect (fun p -> findPath { Current = p; Visited = current :: visited; Remaining = newRemaining })

let content =
    "ln-nr
ln-wy
fl-XI
qc-start
qq-wy
qc-ln
ZD-nr
qc-YN
XI-wy
ln-qq
ln-XI
YN-start
qq-XI
nr-XI
start-qq
qq-qc
end-XI
qq-YN
ln-YN
end-wy
qc-nr
end-nr"

let paths =
    parse content

let combinations = possbileCombinations paths

init combinations
|> List.collect findPath
|> List.length