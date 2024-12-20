﻿open System.Text.RegularExpressions
open Common
open FSharpPlus

// This solution is tenuously functional

let rotate90 m =
    let h, w = Array2D.length1 m, Array2D.length2 m
    Array2D.init w h (fun r c -> m[h - c - 1, r])

let rows m =
    let h, w = Array2D.length1 m, Array2D.length2 m

    // If it's in a list comprehension it's functional right ?
    [ for k in 0 .. h - 1 -> [ for j in 0 .. w - 1 -> m[k, j] ] ]

let diagonals m =
    let h, w = Array2D.length1 m, Array2D.length2 m

    // Yep!
    [ for k in 0 .. (h + w - 2) ->
          [ for j in 0..k do
                if (k - j < h) && (j < w) then
                    yield m[k - j, j] ] ]

let countXmasInLine l =
    let regex = Regex("XMAS", RegexOptions.Compiled) // The simplest regex you've ever seen
    String.ofSeq l |> regex.Matches |> _.Count

let rec countAllXmas grid =
    let rec loop rotation grid =
        if rotation = 360 then
            0
        else
            let rowCount = grid |> rows |> List.sumBy countXmasInLine
            let diagCount = grid |> diagonals |> List.sumBy countXmasInLine
            rowCount + diagCount + loop (rotation + 90) (rotate90 grid)

    loop 0 grid

let aCrosses m =
    let h, w = Array2D.length1 m, Array2D.length2 m

    // I am done pretending I am writing FP code
    seq {
        for k in 1 .. h - 2 do
            yield!
                seq {
                    for j in 1 .. w - 2 do
                        if m[k, j] = 'A' then
                            yield
                                (String.ofList [ m[k - 1, j - 1]; 'A'; m[k + 1, j + 1] ],
                                 String.ofList [ m[k + 1, j - 1]; 'A'; m[k - 1, j + 1] ])
                }
    }

let countAllCrossMas grid =
    grid
    |> aCrosses
    |> Seq.filter (fun (l, r) -> (l = "SAM" || l = "MAS") && (r = "SAM" || r = "MAS"))
    |> Seq.length

aoc {
    day 4

    mapLines (List.map String.toList >> array2D)

    part1 countAllXmas
    part2 countAllCrossMas
}
