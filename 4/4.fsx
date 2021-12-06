let ex = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"

let readAllLines filePath = System.IO.File.ReadAllLines(filePath)

let split (ch: char) (s:string) =
  s.Split([|ch|], System.StringSplitOptions.RemoveEmptyEntries)

let splitOn test lst =
    Array.foldBack (fun el lst ->
            match lst with
            | [] -> [[el]]
            | xs::ys when not (test el) -> (el::xs)::ys
            | _ -> []::lst
         )  lst []

let getRows array = seq {
    for i in 0..Array2D.length1 array - 1 do yield array[i, *]
}

let getColumns array = seq {
    for i in 0..Array2D.length2 array - 1 do yield array[*, i]
}

let flatten array =
  getRows array |> Seq.concat

//let input = ex.Split [|'\n'|]
let input = readAllLines "input.txt"

let createBoard s =
  s |> List.map (fun y -> y |> split ' ')
    |> array2D
    |> Array2D.map (fun x -> (int x, false))

let markBoard number board  =
  board |> Array2D.map (fun (x,m) -> if number = x then (x,true) else (x,m))


let boardWins board =
  let matches elem = elem |> Array.forall (fun (_,m) -> m)
  let rowMatch = getRows board |> Seq.exists matches
  let colMatch = getColumns board |> Seq.exists matches
  if (rowMatch || colMatch) then Some(board) else None

let scoreWinningBoard board =
  flatten board |> Seq.filter (fun (_,m) -> not m) |> Seq.sumBy (fun (x,_) -> x)

let rec part1 boards (numbersLeft: int array) =
  let num = Array.head numbersLeft
  let markedBoards = boards |> List.map (markBoard num)
  let winner = markedBoards |> List.tryPick boardWins
  match winner with
    | Some w -> scoreWinningBoard w * num
    | None -> part1 markedBoards (Array.tail numbersLeft)

let rec part2 boards (numbersLeft: int array) =
  let num = Array.head numbersLeft
  let markedBoards = boards |> List.map (markBoard num)
  let keepBoards = markedBoards |> List.filter (fun b -> (boardWins b).IsNone)
  match keepBoards with
    | [_] -> part1 keepBoards (Array.tail numbersLeft)
    | _ -> part2 markedBoards (Array.tail numbersLeft)


let numbers = input[0].Split [|','|] |> Array.map int
let boards = input[2..] |> splitOn (fun y -> y = "") |> List.map createBoard

part1 boards numbers |> printfn "%A"

part2 boards numbers |> printfn "%A"