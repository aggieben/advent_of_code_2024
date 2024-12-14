open System
open System.IO
open System.Text.RegularExpressions

let inputFile = "03/03_input.txt"

let input = File.ReadAllText inputFile

let parseText (text:string) =
    let matches = Regex.Matches(text, @"mul\(([0-9]{1,3}),([0-9]{1,3})\)", RegexOptions.Multiline)
    matches |> Seq.map (fun m -> int m.Groups.[1].Value, int m.Groups.[2].Value)

input |> parseText |> Seq.sumBy (fun (a,b) -> a * b)

type Operation =
    | Mul of int * int
    | Do
    | Dont

let parseText2 (text:string) =
    let rx = @"mul\(([0-9]{1,3}),([0-9]{1,3})\)|(do)\(\)|(don't)\(\)"
    let matches = Regex.Matches(text, rx)

    matches
    |> Seq.map (fun m ->
                    if m.Groups[0].Value.StartsWith("mul") then Mul (int m.Groups.[1].Value, int m.Groups.[2].Value)
                    elif m.Groups.[0].Value = "do()" then Do
                    elif m.Groups.[0].Value = "don't()" then Dont
                    else failwith "Invalid operation")

let doops ops =
    ({| enabled = true; result = 0 |}, ops)
    ||> Seq.fold (fun acc op ->
                    match op with
                    | Mul (a,b) -> if acc.enabled then {| acc with result = acc.result + a * b |} else acc
                    | Do -> {| acc with enabled = true |}
                    | Dont -> {| acc with enabled = false |})

input |> parseText2 |> doops

