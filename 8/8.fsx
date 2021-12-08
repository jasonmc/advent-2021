#r "nuget: FParsec"
open FParsec

let ex = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

let realInput = System.IO.File.ReadLines "input.txt"

let parseLine str =
    let backtrackingSepBy1 p sep = pipe2 p (many (sep >>? p)) (fun hd tl -> hd::tl)
    let listP = (backtrackingSepBy1 (many1Chars asciiLetter) (pchar ' '))
    let parser = listP .>> skipString " | " .>>. listP
    match run parser str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg


let matchLength (s:string) =
    List.contains s.Length [2; 3; 4; 7]

let partOne data =
    data |> Seq.map (fun (_,y) -> y |> Seq.filter matchLength |> Seq.length) |> Seq.sum


ex.Split '\n' |> Seq.map parseLine |> partOne |> printfn "%A"
realInput |> Seq.map parseLine |> partOne |> printfn "%A"

type SegmentConnection = char * char list

let update (elem: Set<char>) (mustBe: char list) ((c,possible): SegmentConnection) =
    let mb = mustBe |> Set.ofSeq
    (c, if elem.Contains c then possible |> List.filter mb.Contains else possible)

let updateState (currentState: SegmentConnection list) (elem: string) =
    let chars = elem |> Set.ofSeq
    match elem.Length with
    | 2 -> currentState |> List.map (update chars ['c';'f']) // 1
    | 3 -> currentState |> List.map (update chars ['a';'c';'f']) // 7
    | 4 -> currentState |> List.map (update chars ['b';'c';'d';'f']) // 4
    | _ -> currentState


let numberFromSegments segments =
    match segments |> Seq.sort |> Array.ofSeq |> System.String with
    | "abcefg" -> Some '0'
    | "cf" -> Some '1'
    | "acdeg" -> Some '2'
    | "acdfg" -> Some '3'
    | "bcdf" -> Some '4'
    | "abdfg" -> Some '5'
    | "abdefg" -> Some '6'
    | "acf" -> Some '7'
    | "abcdefg" -> Some '8'
    | "abcdfg" -> Some '9'
    | _ -> None

let translate (m: Map<char,char>) x =
    x |> List.map (fun t -> t |> Seq.map (fun c -> m[c]) |> Array.ofSeq |>System.String)
      |> List.map numberFromSegments 


let rec search training (conn: SegmentConnection list) =
    if conn |> List.forall (fun (_,y) -> List.length y = 1) then
        // validate
        let final = conn |> Seq.map (fun (c, cl) -> (c, cl |> List.exactlyOne)) |> Map.ofSeq
        if translate final training |> Seq.forall (fun x -> x.IsSome) then Some final else None
    else
        // get smallest list greater than one, pick each of numbers in it, filter from others, rec on that until find a matching
        let i, j = conn |> List.partition (fun (_,y) -> y.Length <> 1)
        let notSelectable = j |> List.map (fun (_, cl) -> cl |> List.exactlyOne)
        let (e,rest) :: tail = (i |> List.sortBy (fun (_,y) -> List.length y)) @ j
        rest |> Seq.except notSelectable
             |> Seq.map (fun r -> search training ((e,[r]) :: (tail |> List.map (fun (c,l) -> (c, l |> List.except [r])))))
             |> Seq.tryPick id


let partTwo (training, actual) =
    let chars = ['a';'b';'c';'d';'e';'f';'g']
    let startState = chars |> List.map (fun c -> (c, chars))
    let trained =  training |> List.fold updateState startState |> search training |> Option.get
    actual |> translate trained |> List.map Option.get |> Array.ofList |> System.String |> int


ex.Split '\n' |> Seq.map parseLine |> Seq.map partTwo |> Seq.sum |> printfn "%A"
realInput |> Seq.map parseLine |> Seq.map partTwo |> Seq.sum |> printfn "%A"