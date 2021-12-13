let ex = "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"

let realInput = System.IO.File.ReadAllLines "input.txt"

let flatten array = seq {
  for r = 0 to Array2D.length1 array - 1 do
    for c = 0 to Array2D.length2 array - 1 do
      yield array[r,c]
}

let getAdjacent (grid: 'a [,]) i j = seq {
    if i > 0 then
      yield grid[i-1, j] //up
      if j > 0 then
          yield grid[i-1, j-1]
      if j < (Array2D.length2 grid - 1) then
        yield grid[i-1, j+1]
    if i < (Array2D.length1 grid - 1) then
        yield grid[i+1, j] //down
        if j > 0 then
          yield grid[i+1, j-1]
        if j < (Array2D.length2 grid - 1) then
            yield grid[i+1, j+1]
    if j > 0 then
        yield grid[i, j-1] //left
    if j < (Array2D.length2 grid - 1) then
        yield grid[i, j+1] //right
}

let parseGrid =
    array2D >> Array2D.map (string >> int)


let incrCellFromFlashes grid i j (v, _) =
    if v = 0 then
        (v, true)
    else
        let c = getAdjacent grid i j |> Seq.filter (fun (n, f) -> n = 0 && not f) |> Seq.length
        let t = v + c
        ((if t > 9 then 0 else t), false)

let rec flashGrid count grid =
    if grid |> flatten |> Seq.forall (fun (n, y) -> n <> 0 || y)  then
        let allFlashing = grid |> flatten |> Seq.forall (fun (_, y) -> y)
        (grid, count, allFlashing)
    else
        let flashCount = grid |> flatten |> Seq.filter (fun (n, y) -> n = 0 && not y) |> Seq.length
        let ng = grid |> Array2D.mapi (incrCellFromFlashes grid)
        flashGrid (count + flashCount) ng

let doStep (grid, c, _) _ =
    let (g,r,a) = grid |> Array2D.map (fun x -> (if x >= 9 then 0 else x + 1), false)
                       |> flashGrid c
    (g |> Array2D.map fst, r, a)

let partOne lines =
    let init = (parseGrid lines, 0, false)
    let (_,x,_) = [1..100] |> Seq.fold doStep init
    x

ex.Split '\n' |> partOne |> printfn "%A"
realInput |> partOne |> printfn "%A"

let partTwo lines =
    let init = (parseGrid lines, 0, false)
    Seq.initInfinite id |> Seq.scan doStep init
                        |> Seq.takeWhile (fun (_,_,all) -> not all)
                        |> Seq.length

ex.Split '\n' |> partTwo |> printfn "%A"
realInput |> partTwo |> printfn "%A"