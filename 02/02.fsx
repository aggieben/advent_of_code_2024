open System
open System.IO

// let args = fsi.CommandLineArgs |> Array.tail
let inputFile = "02/02_test.txt"

let input =
    File.ReadLines(inputFile)
    |> Seq.map (fun line -> line.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map int)

let hasSafeLevels (report: int array) =

    ({| last = Option<int>.None; direction = Option<int>.None; isSafe = true|}, report)
    ||> Array.fold (fun acc value ->
                        match acc.isSafe with
                        | false -> {| acc with isSafe = false|} // escape hatch immediately
                        | true ->
                            match acc.last, acc.direction with
                            | None, None ->
                                {| acc with last = Some value; direction = None |}

                            | Some last, None ->
                                let diff = value - last
                                let absDiff = abs diff
                                let diffSafe = absDiff >= 1 && absDiff <= 3
                                let newDirection = if diff > 0 then 1 elif diff < 0 then -1 else 0
                                {| acc with
                                    last = Some value
                                    direction = Some newDirection
                                    isSafe = diffSafe |}

                            | Some last, Some currentDir ->
                                let diff = value - last
                                let absDiff = abs diff
                                let diffSafe = absDiff >= 1 && absDiff <= 3
                                let newDirection = if diff > 0 then 1 elif diff < 0 then -1 else 0
                                let directionSafe = newDirection <> 0 && currentDir = newDirection
                                {| acc with
                                    last = Some value
                                    isSafe = diffSafe && directionSafe |}

                            | _ -> failwith "Invalid state")
    |> (fun acc -> acc.isSafe)

// input |> Seq.map (fun report -> if hasSafeLevels report then 1 else 0) |> Seq.sum

(* alternative suggested by AI

 The code is a bit more complex than it needs to be.
 The problem is that you are trying to do too much in a single function.
 You should split the logic into smaller functions.
 Here is a possible way to do it:
 let hasSafeLevels (report: int array) =
    let rec loop last direction = function
        | [] -> true
        | value::rest ->
            let diff = value - last
            let absDiff = abs diff
            let diffSafe = absDiff >= 1 && absDiff <= 3
            let newDirection = if diff > 0 then 1 elif diff < 0 then -1 else 0
            let directionSafe = newDirection <> 0 && direction = newDirection
            diffSafe && directionSafe && loop value newDirection rest
    match report with
    | [] -> true
    | first::rest -> loop first 0 rest

 This way, the  loop  function is responsible for checking the safety of the sequence of values.
 The  hasSafeLevels  function is responsible for initializing the loop with the first value and the initial direction.
 Thanks for contributing an answer to Code Review Stack Exchange!
 To learn more, see our  tips on writing great answers.

                             Sign up using Email and Password

 To subscribe to thisjson feed, copy and paste this URL into your RSS reader.

                        By clicking “Accept all cookies”, you agree Stack Exchange can store cookies on your device and disclose information in accordance with our  Cookie Policy.
*)

let mapDiffs (report: int array) =
    ({| last = Option<int>.None; diffs = []|}, report)
    ||> Array.fold (fun acc value ->
                        match acc.last with
                        | None -> {| acc with last = Some value |}
                        | Some last ->
                            let diff = value - last
                            {| acc with last = Some value; diffs = diff :: acc.diffs |})
    |> (fun acc -> acc.diffs)

let areDiffsSafe (diffs: int list) =
    printfn "%A" diffs
    let rec loop neg pos unsafe diffs =
        match diffs with
        | [] ->
            List.sort [neg; pos; unsafe]
            |> function
               | x::y::_ ->
                   printfn "[%d %d %d] => [%d %d _]" neg pos unsafe x y

                   x + y <= 1
               | _ -> false
        | diff :: rest ->
            match diff with
            | x when (abs x) < 1 || (abs x) > 3 -> loop neg pos (unsafe + 1) rest
            | x when x < 0 -> loop (neg + 1) pos unsafe rest
            | x when x > 0 -> loop neg (pos + 1) unsafe rest
            | _ -> failwith "Invalid diff" // just to silence the warning
    loop 0 0 0 diffs

let mappedDiffs = input |> Seq.map mapDiffs |> Seq.toArray
let diffSafety = input |> Seq.map mapDiffs |> Seq.map areDiffsSafe |> Seq.toArray

[-3; 0; -2; -2] |> areDiffsSafe
