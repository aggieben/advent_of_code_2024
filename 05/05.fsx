open System
open System.IO
open System.Text

let inputFile = "05/05_test.txt"

let input = File.ReadLines inputFile

// fun line -> line.Split("|") |> (fun arr -> int arr[0], int arr[1])

let parsedInput =
    ({| inRules = true; rules = []; updates = []|}, input)
    ||> Seq.fold (fun acc line ->
                    if acc.inRules then
                        if line <> "" then
                            let (a, b) = line.Split("|") |> (fun arr -> int arr[0], int arr[1])
                            {| acc with rules = (a, b) :: acc.rules |}
                        else
                            {| acc with inRules = false |}
                    else
                        {| acc with updates = (line.Split(",") |> Array.map int) :: acc.updates |})

let rules = parsedInput.rules
let updates = parsedInput.updates

let insertMapList key value map =
    match Map.tryFind key map with
    | Some list -> Map.add key (value :: list) map
    | None -> Map.add key [value] map

let ruleMap = parsedInput.rules |> List.fold (fun map (a, b) -> insertMapList a b map) Map.empty

