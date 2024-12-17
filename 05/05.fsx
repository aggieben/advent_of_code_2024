open System
open System.IO
open System.Text

let inputFile = "05/05_input.txt"

let input = File.ReadLines inputFile

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

let updateIsValid update =
    let updateMap = update |> Array.mapi (fun i v -> (v, i)) |> Map.ofArray

    // the semantic here is "are all pages in the update well-ordered?"
    update |>
        Array.forall (
            fun page ->
                match Map.tryFind page ruleMap with
                | Some subsequents ->
                    subsequents |> List.forall (fun sub ->
                                                    match Map.tryFind sub updateMap with
                                                    | Some subIndex -> subIndex > updateMap[page]
                                                    | None -> true)
                | None -> true)

let validUpdates =
    updates
    |> List.filter updateIsValid

validUpdates |> List.sumBy (fun update -> update[(update.Length - 1)/2])

