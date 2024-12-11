#nowarn 0025

open Common
open FSharpPlus

let rec comb n l =
    match n, l with
    | 0, _ -> [ [] ]
    | _, [] -> []
    | k, x :: xs -> List.map ((@) [ x ]) (comb (k - 1) xs) @ comb k xs

let antinodePart1 (x1, y1) (x2, y2) = (2.0 * x2 - x1, 2.0 * y2 - y1)

let antinodesPart2 (w, h) (x1, y1) (x2, y2) =
    let f x = (y2 - y1) / (x2 - x1) * (x - x1) + y1
    let isCloseToInteger y = abs (y - round y) < 1e-6 // Account for floating point errors

    List.init w (fun x -> x, f (float x))
    |> List.filter (fun (_, y) -> isCloseToInteger y && y >= 0.0 && y < float h)

aoc {
    day 8

    mapLines (fun lines ->
        let antennas =
            lines
            |> List.mapi (fun y line -> String.toList line |> List.mapi (fun x c -> c, (float x, float y)))
            |> List.concat
            |> List.filter (fun (c, _) -> c <> '.')
            |> List.groupBy fst
            |> List.map (fun (_, l) -> l |> List.map snd)

        let h = List.length lines
        let w = List.head lines |> String.length
        (w, h), antennas)

    part1 (fun ((w, h), antennas) ->
        antennas
        |> List.collect (fun positions ->
            List.allPairs positions positions // This way we get both (a, b) and (b, a)
            |> List.filter (fun (a, b) -> a <> b)
            |> List.map (fun (a, b) -> antinodePart1 a b)
            |> List.filter (fun (x, y) -> x >= 0 && x < w && y >= 0 && y < h))
        |> List.distinct
        |> List.length)

    part2 (fun ((w, h), antennas) ->
        antennas
        |> List.collect (fun positions -> comb 2 positions |> List.collect (fun [ a; b ] -> antinodesPart2 (w, h) a b))
        |> List.distinct
        |> List.length)
}
