open System
open System.IO

let sortedInsert (list: int list) (value: int) =
    let rec insert (list: int list) (value: int) =
        match list with
        | [] -> [value]
        | x :: xs -> if x > value then value :: x :: xs else x :: insert xs value
    insert list value

let parseLine (line: string) =
    let space = line.IndexOf(" ")
    let first = line.AsSpan(0, space)
    let second = line.AsSpan(space + 1)
    (Int32.Parse(first), Int32.Parse(second))


let (sortedList1, sortedList2) =
    File.ReadLines("01_input.txt")
    |> Seq.fold (fun acc line ->
        let (first, second) = parseLine line
        (first |> sortedInsert (fst acc), second |> sortedInsert (snd acc))
    ) (List.empty<int>, List.empty<int>)

let totalDistance =
    (sortedList1, sortedList2)
    ||> List.fold2 (fun sum (x: int) (y: int) -> sum + Math.Abs(x - y)) 0


let sumMatches (value: int) (list: int list) =
    let mutable sum = 0
    for i in list do
        if i = value then
            sum <- sum + value
    sum

let similarity =
    sortedList1
    |> List.fold (fun totalSum value -> totalSum + sumMatches value sortedList2) 0

(totalDistance, similarity)
