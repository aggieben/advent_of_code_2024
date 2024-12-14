open System
open System.IO

// let args = fsi.CommandLineArgs |> Array.tail
let inputFile = "02/02_input.txt"

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

let hasSafeLevels2 (report: int list) =
    printfn "%A" report
    let rec loop (last:int) (direction:int option) (canEliminate:bool) =
        function
        | [] -> true
        | value::rest ->
            let diff = value - last
            let absDiff = abs diff
            if absDiff = 0
            then
                if canEliminate then loop last direction false rest// drop this value and continue
                else false
            else
                let diffSafe = absDiff >= 1 && absDiff <= 3
                if not diffSafe
                then
                    if canEliminate then loop last direction false rest // drop this value and continue
                    else false
                else
                    let newDirection = if diff > 0 then 1 else -1
                    match direction with
                    | Some dir ->
                        if dir <> newDirection
                        then
                            if canEliminate then loop last direction false rest // drop this value and continue
                            else false
                        else
                            loop value (Some newDirection) canEliminate rest
                    | None ->
                        loop value (Some newDirection) canEliminate rest

    match report with
    | [] -> true
    | first::rest ->
        loop first None true rest
        |> (fun b -> printfn "%A" b; b)

input
|> Seq.map List.ofSeq
// this is a stupid hack for dealing with the case where the first value needs to be eliminated
|> Seq.map (fun report -> if hasSafeLevels2 report then true else List.rev report |> hasSafeLevels2)
|> Seq.sumBy (fun b -> if b then 1 else 0)

(*
probably the better way to handle this is to initialize the loop function with a
None instead of the first value; hacking this to double the work to catch the
first value is not a good computational tradeoff
*)