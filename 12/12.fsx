let ex = "start-A
start-b
A-c
A-b
b-d
A-end
b-end"

let realInput = System.IO.File.ReadAllLines "input.txt"

let isUpper str =
    str |> Seq.forall System.Char.IsUpper

type 'a Edge = 'a * 'a

let parse (l: string) =
    let x = l.Split '-'
    Edge (x[0], x[1])

let edgesToGraph  (edges: 'a Edge seq) =
    let n1, n2 = edges |> Seq.toList |> List.unzip
    let nodeMap = n1 @ n2 |> List.distinct |> List.map(fun n -> n, []) |> Map.ofList
    edges |> Seq.fold (fun map (a, b) -> map |> Map.add a (b::map[a]) |> Map.add b (a::map[b])) nodeMap


let calculatePaths from dest partTwo (map: Map<string, string list>) =
    let rec calculate path visited smallTwice =
        let curr = List.head path
        if curr = dest then
            path |> List.rev |> Seq.singleton
        else
            let visited = Set.add curr visited
            map[curr] |> Seq.collect (getNext path visited smallTwice)
    and getNext path visited smallTwice x =
        if isUpper x || not (Set.contains x visited) then
            calculate (x::path) visited smallTwice
        else if x <> from && not smallTwice then
            calculate (x::path) visited true
        else
            Seq.empty
        
    calculate [from] Set.empty (not partTwo)


let partOne lines =
    lines |> Seq.map parse |> edgesToGraph |> calculatePaths "start" "end" false |> Seq.length

ex.Split '\n' |> partOne |> printfn "%A"
realInput |> partOne |> printfn "%A"

let partTwo lines =
    lines |> Seq.map parse |> edgesToGraph |> calculatePaths "start" "end" true |> Seq.length

ex.Split '\n' |> partTwo |> printfn "%A"
realInput |> partTwo |> printfn "%A"