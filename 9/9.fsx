let ex = "2199943210
3987894921
9856789892
8767896789
9899965678"


let realInput = System.IO.File.ReadAllLines "input.txt"

let flatten array = seq {
  for r = 0 to Array2D.length1 array - 1 do
    for c = 0 to Array2D.length2 array - 1 do
      yield array[r,c]
}

type Cell<'a> = Cell of int * int * 'a * (Cell<'a> seq)

let rec getAdjacent (cave: 'a [,]) i j : Cell<'a> seq = seq{
    if i > 0 then
      yield y cave (i-1) j //up
    if j > 0 then
        yield y cave i (j-1) //left
    if i < (Array2D.length1 cave - 1) then
        yield y cave (i+1) j //down
    if j < (Array2D.length2 cave - 1) then
        yield y cave i (j+1) //right
}
and y (cave: 'a [,]) i j =
    Cell (i, j, cave[i, j], getAdjacent cave i j)

let getAdjacents cave =
    cave |> Array2D.mapi (fun r c v -> Cell(r,c,v, getAdjacent cave r c))

let parseCave =
    array2D >> Array2D.map (string >> int)

let getLowPoints cell =
    let (Cell (_, _, v, around)) = cell
    if around |> Seq.forall (fun (Cell (_,_,x, _)) -> x > v) then Some cell else None

let partOne input =
    input |> parseCave
          |> getAdjacents
          |> flatten
          |> Seq.choose getLowPoints
          |> Seq.sumBy (fun (Cell (_,_,x, _)) -> x+1)

ex.Split '\n' |> partOne |> printfn "%A"
realInput |> partOne |> printfn "%A"

let rec getBasin ((Cell (r,c,v, around)): Cell<int>) =
    // get neighbours that are higher than it (but not 9)
    Cell (r,c,v, around |> Seq.filter (fun (Cell (_,_,x, _)) -> x > v && x < 9)  |> Seq.map getBasin)

let rec flattenCell ((Cell (r,c,v, around)): Cell<'a>) : (int * int * 'a) seq =
    Seq.append [(r,c,v)] (around |> Seq.collect flattenCell)

let partTwo input =
    input |> parseCave
          |> getAdjacents
          |> flatten
          |> Seq.choose getLowPoints
          |> Seq.map getBasin
          |> Seq.map (flattenCell >> Seq.distinct >> Seq.length)
          |> Seq.sortDescending
          |> Seq.take 3
          |> Seq.reduce (*)

ex.Split '\n' |> partTwo |> printfn "%A"
realInput |> partTwo |> printfn "%A"