let ex = "3,4,3,1,2"

let realInput = System.IO.File.ReadLines "input.txt"

let split (ch: char) (s:string) =
  s.Split [|ch|]

let iterateFish f =
    match f with
    | 0 -> [6;8]
    | x -> [x-1]

let calculateNum days init  =
    let init = init |> Seq.map int
    [1..days] |> List.fold (fun gen _ -> gen |> Seq.collect iterateFish) init |> Seq.length


ex |> split ',' |> calculateNum 80 |> printfn "%A"

realInput |> Seq.head |> split ',' |> calculateNum 80 |> printfn "%A"

let getOrDefault defVal (m: Map<'a, 'b>) i =
    m.TryFind i |> Option.defaultValue defVal

let updateMap key updater (m: Map<'a, uint64>) =
    m.Add (key,getOrDefault 0UL m key |> updater)

let createNewMap (oldMap: Map<int, uint64>) newMap i =
    let getOldVal = getOrDefault 0UL oldMap
    match i with
    | 0 -> newMap |> updateMap 6 (fun v -> v + getOldVal 0) |> updateMap 8 (fun _ -> getOldVal 0)
    | x -> newMap |> updateMap (x - 1) (fun v -> v + getOldVal x)

let iterateAllFish state _ =
    [8..-1..0] |> List.fold (createNewMap state) Map.empty


let calculateNumFaster days init =
    let init = init |> Seq.map int |> Seq.countBy id |> Seq.map (fun (x,y) -> (x, uint64 y)) |> Map.ofSeq
    [1..days] |> List.fold iterateAllFish init |> Seq.sumBy (fun x -> x.Value)


ex |> split ',' |> calculateNumFaster 256 |> printfn "%A"

realInput |> Seq.head |> split ',' |> calculateNumFaster 256 |> printfn "%A"