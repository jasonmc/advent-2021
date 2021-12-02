let example = "199
200
208
210
200
207
240
269
260
263"

let readLines filePath = System.IO.File.ReadLines(filePath)

let first = Seq.map int >> Seq.pairwise >> Seq.filter (fun (x,y) -> y > x) >> Seq.length

// example.Split [|'\n'|] |> doIt |> printfn "%d"

readLines "input.txt" |> first |> printfn "%d"

let second x = x |> Seq.map int
                 |> Seq.windowed 3
                 |> Seq.map Seq.sum
                 |> Seq.pairwise
                 |> Seq.filter (fun (x,y) -> y > x)
                 |> Seq.length

readLines "input.txt" |> second |> printfn "%d"
