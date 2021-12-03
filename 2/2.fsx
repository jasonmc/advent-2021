let example = "forward 5
down 5
forward 8
up 3
down 8
forward 2"


let readLines filePath = System.IO.File.ReadLines(filePath)

type Position = int * int
type Position2 = int * int * int

let scorePosition (a,b) = a * b
let scorePosition2 (_, b, c) = b * c


let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(int(s.Substring(p.Length)))
    else
        None

let updatePosition acc elem =
    let f, d = acc
    match elem with
    | Prefix "forward" x -> (f + x, d)
    | Prefix "down" x -> (f,d + x)
    | Prefix "up" x -> (f,d - x)
    | _ -> acc

example.Split [|'\n'|] |> Seq.fold updatePosition (0,0) |> scorePosition |> printfn "%d"


readLines "input.txt" |> Seq.fold updatePosition (0,0) |> scorePosition |> printfn "%d"


let updatePosition2 acc elem =
    let a, f, d = acc
    match elem with
    | Prefix "forward" x -> (a, f + x, d + (a * x))
    | Prefix "down" x -> (a+x, f, d)
    | Prefix "up" x -> (a-x, f, d)
    | _ -> acc

example.Split [|'\n'|] |> Seq.fold updatePosition2 (0,0,0) |> scorePosition2 |> printfn "%d"


readLines "input.txt" |> Seq.fold updatePosition2 (0,0,0) |> scorePosition2 |> printfn "%d"