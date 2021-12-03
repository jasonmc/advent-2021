let ex = "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"

let readLines filePath = System.IO.File.ReadLines(filePath)
let printBin (i: int) = System.Convert.ToString (i, 2)
let parseBin s = System.Convert.ToInt32(s, 2)
let toString : char seq -> string = Seq.map string >> String.concat ""

let update acc elem =
    Seq.zip acc elem |> Seq.map (fun ((z, o), y) -> if y = '1' then (z, o + 1) else (z+1, o))

let calculateRate len  leastCommon input =
    input |> Seq.fold update (Seq.init len (fun _ -> (0,0)))
          |> Seq.map (fun (z,o) -> if z > o <> leastCommon then '0' else '1') |> toString

let partOne len input =
    (calculateRate len  false input |> parseBin) * (calculateRate len  true input |> parseBin)

ex.Split [|'\n'|] |> partOne 5 |> printfn "%d"

readLines "input.txt" |> partOne 12 |> printfn "%d"


let rec calculateRating len leastCommon position (candidates: string seq) =
    if Seq.length candidates = 1 then
        Seq.exactlyOne candidates
    else
        let rate =  candidates |> Seq.map (fun c -> [c[position]]) |> calculateRate len leastCommon |> Seq.exactlyOne
        let newCandidates = candidates |> Seq.filter (fun c -> c[position] = rate)
        calculateRating len leastCommon (position+1) newCandidates


let partTwo len input =
    (calculateRating len false 0 input |> parseBin) * (calculateRating len true 0 input |> parseBin)

ex.Split [|'\n'|] |> partTwo 5 |> printfn "%d"
readLines "input.txt" |> partTwo 12 |> printfn "%d"