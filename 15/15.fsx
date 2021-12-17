let ex = "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"

let realInput = System.IO.File.ReadAllLines "input.txt"

let flatten array = seq {
  for r = 0 to Array2D.length1 array - 1 do
    for c = 0 to Array2D.length2 array - 1 do
      yield array[r,c]
}

[<CustomEquality; CustomComparison>]
type Cell<'a> = Cell of int * int * 'a * (Cell<'a> seq)
                    override x.Equals(y) =
                        let (Cell (i1,j1,_, _)) = x
                        match y with
                        | :? Cell<'a> as (Cell (i2,j2,_, _)) -> ((i1,j1) = (i2,j2))
                        | _ -> invalidArg "y" "wrong type"
                    override x.GetHashCode() =
                        let (Cell (i,j,_, _)) = x
                        hash((i,j))
                    interface System.IComparable with
                        member x.CompareTo obj =
                            let (Cell (i1,j1,_, _)) = x
                            match obj with
                            | :? Cell<'a> as (Cell (i2,j2,_, _)) -> compare (i1,j1) (i2,j2)
                            | _                -> invalidArg "obj" "not a Category"

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

let updateDistanceMap currentNode distances (queue: System.Collections.Generic.PriorityQueue<Cell<int>, int>) nodesToUpdate  =
    let updateDistance distances cell =
        let (Cell(_,_,v,_)) = cell
        let tentativeDistance = Map.find currentNode distances + v
        if tentativeDistance < (Map.find cell distances) then
            queue.Enqueue  (cell, tentativeDistance)
            distances |> Map.add cell tentativeDistance
        else   
            distances
    nodesToUpdate |> Seq.fold updateDistance distances

let rec solve currentNode endNode distances unvisitedNodes (queue: System.Collections.Generic.PriorityQueue<Cell<int>, int>) =
    let (Cell(_,_,_, neighbors)) = currentNode
    if currentNode <> endNode then
        let unvisitedNeighbours = neighbors |> Seq.filter (fun c -> Set.contains c unvisitedNodes)
        let newDistanceMap = unvisitedNeighbours |> updateDistanceMap currentNode distances queue
        let nextCell = queue.Dequeue ()
        let unvisitedNodes = Set.remove nextCell unvisitedNodes
        solve nextCell endNode newDistanceMap unvisitedNodes queue
    else
        Map.find endNode distances

let djikstra (input: int [,]) =
    let cellGrid = getAdjacents input
    let start = cellGrid[0,0]
    let endNode = cellGrid[Array2D.length1 cellGrid - 1, Array2D.length1 cellGrid - 1]
    let cells = cellGrid |> flatten
    let distances = cells |> Seq.map (fun c -> (c, System.Int32.MaxValue))
                          |> Map.ofSeq
                          |> Map.add start 0
    let unvisitedNodes = cells |> Seq.filter ((<>) start) |> Set.ofSeq
    let queue = System.Collections.Generic.PriorityQueue<Cell<int>, int>()
    solve start endNode distances unvisitedNodes queue


let partOne lines =
    lines |> parseCave |> djikstra

ex.Split '\n' |> partOne |> printfn "%A"
realInput |> partOne |> printfn "%A"


let expand r c cellGrid =
    let wrap x = ((x - 1) % 9) + 1
    let len1 = Array2D.length1 cellGrid 
    let len2 = Array2D.length2 cellGrid
    Array2D.init (len1 * r) (len2 * c) (fun i j -> cellGrid[i % len1, j % len2] + (i / len1) + (j / len2) |> wrap)

let partTwo lines =
    lines |> parseCave |> expand 5 5 |> djikstra

ex.Split '\n' |> partTwo |> printfn "%A"
realInput |> partTwo |> printfn "%A"