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
                            let diff =
                                match acc.last with
                                | Some last -> value - last
                                | None -> 0
                            let absDiff = abs diff
                            let direction =
                                if diff > 0 then Some 1
                                elif diff < 0 then Some -1
                                else None

                            {| last = Some value;
                               direction = direction;
                               isSafe = absDiff >= 1 && absDiff <= 3|})
    |> (fun acc -> acc.isSafe)


