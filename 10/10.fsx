let ex = "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"

let realInput = System.IO.File.ReadAllLines "input.txt"

let median data =
    let d = data |> Seq.toArray |> Array.sort
    if d.Length % 2 = 0 then
        failwith "even number"
    else
        d[d.Length / 2]

type S = Stack of char list

let EMPTY = Stack []

let push x (Stack contents) =
    Stack (x::contents)

let pop (Stack contents) = 
    match contents with 
    | top::rest -> Some (top,Stack rest)
    | [] -> None

let popAll (Stack contents) = 
    contents

type State = S * char option

let scoreChar c =
    match c with |')' -> 3 |']' -> 57 |'}' -> 1197 |'>' -> 25137 | _ -> failwith "unknown char"

let scoreChar2 c =
    match c with | ')' -> 1| ']' -> 2 | '}' -> 3 | '>' -> 4 | _ -> failwith "unknown char"


let matches =
    [ '(', ')';'[', ']';'{', '}';'<', '>';]
    |> Map.ofList

let processCh (Stack stack, o) ch =
    match o with
    | Some _ -> (Stack stack, o)
    | None ->
                match ch with
                | x when matches.ContainsKey x -> ((push ch (Stack stack)), None)
                | x -> match pop (Stack stack) with
                        | Some (v,s) -> (s, if matches[v] = x then None else Some x)
                        | None -> ((Stack stack), Some x)

let partOne lines =
    lines |> Seq.map (Seq.fold processCh (EMPTY, None))
          |> Seq.choose (fun (_,y) -> y)
          |> Seq.sumBy scoreChar

ex.Split '\n' |> partOne |> printfn "%A"
realInput |> partOne |> printfn "%A"

let scoreLine line =
    line |> Seq.fold (fun s e -> (s * 5UL) + uint64(scoreChar2 e)) 0UL

let partTwo lines =
    let scores = lines |> Seq.map (Seq.fold processCh (EMPTY, None))
                       |> Seq.filter (fun (_,y) -> y.IsNone)
                       |> Seq.map (fun ((Stack stack),_) -> stack |> List.map (fun v -> matches[v]) |> scoreLine)
    median scores
    

ex.Split '\n' |> partTwo |> printfn "%A"
realInput |> partTwo |> printfn "%A"