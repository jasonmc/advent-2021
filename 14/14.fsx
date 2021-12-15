#r "nuget: FParsec"
open FParsec

let ex ="NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
"

let realInput = System.IO.File.ReadAllText "input.txt"

let parse str =
    let instr = asciiUpper .>>. asciiUpper .>> skipString " -> " .>>. asciiUpper  .>> newline
    let parser = (many asciiUpper) .>> skipNewline .>>  skipNewline .>>. (many instr) .>> eof
    match run parser str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg


let doPass (rulesMap: Map<(char*char), char>) input _ =
    let r = input |> List.pairwise |> List.collect (fun (x, y) -> [x; rulesMap[(x, y)]])
    r @ [List.last input]

let partOne input =
    let template, rules = parse input
    let rulesMap = rules |> Map.ofList
    let final = [1..10] |> List.fold (doPass rulesMap) template
                        |> Seq.groupBy id
                        |> Seq.map (snd >> Seq.length)
    (Seq.max final) - (Seq.min final)

ex |> partOne |> printfn "%A"
realInput |> partOne |> printfn "%A"

let doPass2 (rulesMap: Map<(char*char), char>) input _ =
    let newPairs = input |> List.collect (fun ((x, y), c) -> [((x, rulesMap[(x, y)]), c); ((rulesMap[(x, y)], y), c)])
    newPairs |> List.groupBy fst
             |> List.map (fun (x, y) -> (x, y |> List.sumBy snd))

let partTwo input =
    let template, rules = parse input
    let rulesMap = rules |> Map.ofList
    let start = template |> List.pairwise |> List.map (fun x -> (x, 1UL))
    let final = [1..40] |> List.fold (doPass2 rulesMap) start
                        |> List.map (fun ((x, _),c) -> (x,c))
    let firstChars = final @ [template |> Seq.last, 1UL]
    let counts = firstChars |> List.groupBy fst
                            |> List.map (fun x -> x |> snd |> List.sumBy snd)
    (List.max counts) - (List.min counts)


ex |> partTwo |> printfn "%A"
realInput |> partTwo |> printfn "%A" 