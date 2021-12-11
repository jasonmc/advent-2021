let ex = "2199943210
3987894921
9856789892
8767896789
9899965678"


let realInput = System.IO.File.ReadAllLines "input.txt"

let getRows array = seq {
    for i in 0..Array2D.length1 array - 1 do yield array[i, *]
}

let getColumns array = seq {
    for i in 0..Array2D.length2 array - 1 do yield array[*, i]
}

let flatten array =
    getRows array |> Seq.concat


let getAdjacent (cave: 'a [,]) i j = seq{
    if i > 0 then
      yield cave[i-1, j] //up
    if j > 0 then
        yield cave[i, j-1] //left
    if i < (Array2D.length1 cave - 1) then
        yield cave[i+1, j] //down
    if j < (Array2D.length2 cave - 1) then
        yield cave[i, j+1] //right
}

// let getAdjacents cave = seq {
//   for r = 0 to Array2D.length1 cave - 1 do
//     for c = 0 to Array2D.length2 cave - 1 do
//       yield getAdjacent cave r c
// }

let getAdjacents cave =
    cave |> Array2D.mapi (fun r c v -> (v,getAdjacent cave r c))

let smallestAround ((v:int), (around: int seq)) =
    if around |> Seq.forall (fun x -> x > v) then Some v else None


let parseCave (s: string seq) =
    s |> array2D |> Array2D.map (string >> int)


let partOne input =
    input |> parseCave
          |> getAdjacents
          |> Array2D.map smallestAround
          |> flatten
          |> Seq.choose id
          |> Seq.sumBy (fun x -> x+1)

ex.Split '\n' |> partOne |> printfn "%A"
realInput |> partOne |> printfn "%A"