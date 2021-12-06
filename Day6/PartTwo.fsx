open System.IO

let toMap list =
    [0..8]
    |> List.map (fun daysLeft ->
        let count =
            list
            |> List.filter ((=) daysLeft)
            |> List.length
            |> int64
        daysLeft, count)
    |> Map.ofList
    
let afterDay m =
    m
    |> Map.map (fun daysLeft _ ->
        match daysLeft with
        | 8 -> Map.find 0 m
        | 6 -> Map.find 7 m + Map.find 0 m
        | n -> Map.find (n + 1) m)

// let content = "3,4,3,1,2"

let content = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

let init =
    content.Split(',')
    |> Seq.map int
    |> Seq.toList

[1..256]
|> List.fold (fun acc _ -> afterDay acc) (toMap init)
|> Map.values
|> Seq.sum