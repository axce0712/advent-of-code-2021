open System.IO

File.ReadLines (Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
|> Seq.map int
|> Seq.windowed 3
|> Seq.map Array.sum
|> Seq.pairwise
|> Seq.sumBy (fun (a, b) -> if b > a then 1 else 0)