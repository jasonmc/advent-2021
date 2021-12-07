let ex = "16,1,2,0,4,2,7,1,2,14"

let realInput = System.IO.File.ReadLines "input.txt"

let split (ch: char) (s:string) = s.Split ch

let binomial n = n*(n+1)/2

let median (data : int array) =
    let data = Array.sort data
    let num = data.Length / 2;
    if data.Length % 2 = 0 then
        (data[num - 1] + data[num]) / 2
    else
        data[num]


let calculate input =
    let nums = input |> split ',' |> Array.map int
    let med = median nums
    nums |> Array.map int |> Seq.fold (fun agg e -> agg + abs(med - e)) 0

ex |> calculate |> printfn "%A"
realInput |> Seq.head |> calculate |> printfn "%A"


let distanceTwo x m =
    x |> Seq.fold (fun agg e -> agg + binomial (abs(m - e))) 0

let bruteForceTwo input =
    let nums = input |> split ',' |> Array.map int
    [Seq.min nums .. Seq.max nums] |> Seq.map (distanceTwo nums) |> Seq.min

ex |> bruteForceTwo |> printfn "%A"
realInput |> Seq.head |> bruteForceTwo |> printfn "%A"