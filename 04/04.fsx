open System
open System.IO
open System.Text

let inputFile = "04/04_input.txt"

let input = File.ReadAllLines inputFile |> Array.map Encoding.UTF8.GetBytes

// X: 0x58=88; M: 0x4d=77; A: 0x41=65; S: 0x53=83
let word = [ 0x58uy; 0x4duy; 0x41uy; 0x53uy ]

let inline (+) origin change =
    fst origin + fst change, snd origin + snd change

let coordValid (bytes:byte[][]) coord =
    fst coord >= 0 && fst coord < Array.length bytes &&
    snd coord >= 0 && snd coord < Array.length bytes[0]

let rec isWordOnVector (bytes:byte[][]) coord vec word =
    // printfn "isWordOnVector %A %A %A" coord vec word
    if List.isEmpty word then true
    elif not (coordValid bytes coord) then false
    elif bytes[fst coord][snd coord] <> List.head word then false
    else
        isWordOnVector bytes (coord + vec) vec (List.tail word)

let findWordsFrom (bytes:byte[][]) row col =
    // printfn "findWordsFrom %A %A" row col
    let vecs = [
        (0, 1); (0, -1); (1, 0); (-1, 0)
        (1, 1); (1, -1); (-1, 1); (-1, -1)
    ]

    vecs |> Seq.sumBy (fun vec -> if isWordOnVector bytes (row, col) vec word then 1 else 0)

let iterate2d rowCount colCount =
    seq {
        for row in 0 .. rowCount - 1 do
            for col in 0 .. colCount - 1 do
                yield row, col
    }

let findWords (bytes:byte[][]) =
    iterate2d (Array.length bytes) (Array.length bytes[0])
    |> Seq.sumBy (fun (row, col) -> findWordsFrom bytes row col)
    // |> Seq.iter (fun c -> printfn "%A" c)

findWords input
