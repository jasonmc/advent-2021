#r "nuget: FParsec"
open FParsec

let ex = "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
"

let realInput = System.IO.File.ReadAllText "input.txt"

let parse str =
    let dot = pint32 .>> skipChar ',' .>>. pint32 .>> newline
    let fold = skipString "fold along " >>. anyChar .>> skipChar '=' .>>. pint32  .>> newline
    let parser = (many dot) .>> skipNewline .>>. (many fold) .>> eof
    match run parser str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg

let wrap max fold x =
    // fold must be max/2
    if x > fold then (max - x)
    else x

let doFold dots fold =
    let maxX = dots |> Seq.map fst|> Seq.max
    let maxY = dots |> Seq.map snd |> Seq.max
    match fold with
    | 'x', d -> dots |> Seq.map (fun (x,y) -> (wrap maxX d x, y))
    | 'y', d -> dots |> Seq.map (fun (x,y) -> (x, wrap maxY d y))
    | _ -> failwith ""

let partOne input =
    let (dots, folds) = parse input
    let firstFold = folds |> Seq.head
    firstFold |> doFold dots |> Seq.distinct |> Seq.length

let plot dots = seq {
    let dotSet = dots |> Set.ofSeq
    for y = 0 to dots |> Seq.map snd |> Seq.max do
        for x = 0 to dots |> Seq.map fst |> Seq.max do
            yield if Set.contains (x,y) dotSet then '#' else '.'
        yield '\n'
    }

let partTwo input =
    let (dots, folds) = parse input
    folds |> Seq.fold doFold dots |> plot |> Array.ofSeq |> System.String


ex |> partOne |> printfn "%A"
realInput |> partOne |> printfn "%A"

ex |> partTwo |> printfn "%s"
realInput |> partTwo |> printfn "%s"