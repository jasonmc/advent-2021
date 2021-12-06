#r "nuget: FParsec"
open FParsec

let ex = "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"

let readLines filePath = System.IO.File.ReadLines(filePath)

let parseLine str =
    let parser = pint32 .>> skipChar ',' .>>. pint32 .>> skipString " -> " .>>. (pint32 .>> skipChar ',' .>>. pint32)
    match run parser str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg

let range x1 x2 =
    [x1..(if x2 < x1 then -1 else 1)..x2]

let allPoints ((x1,y1),(x2,y2)) = seq {
    if x1 = x2 || y1=y2 then //straight
        for i in range x1 x2 do 
            for j in range y1 y2 do
                yield (i, j)
    else // diagonal
        let dx = x2 - x1
        let dy = y2 - y1
        let n = if dx = dy then 1 else -1
        for i in range x1 x2 do
            yield (i, y1 + ((i - x1) * n))

}

let partTwo input =
    input |> Seq.map parseLine
          |> Seq.collect allPoints 
          |> Seq.countBy id
          |> Seq.filter (fun (_,y) -> y > 1) 
          |> Seq.length
          

ex.Split [|'\n'|] |> partTwo |> printfn "%A"
readLines "input.txt" |> partTwo |> printfn "%A"
