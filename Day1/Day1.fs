#nowarn 0025

open Common.Runner
open FSharpPlus
        
let findTotalDistanceSum (left, right) =
    let left = List.sort left
    let right = List.sort right

    (left, right) ||> List.map2 (-) |> List.sumBy abs

let findTotalSimilaritySum (left, right) =
    let appearances = right |> List.countBy id |> Map.ofList

    left
    |> List.sumBy (fun v -> v * (appearances |> Map.tryFind v |> Option.defaultValue 0))

aoc {
    day 1

    mapLines (List.map (String.split [ "   " ] >> Seq.toList >> fun [ l; r ] -> int l, int r) >> List.unzip)

    part1 findTotalDistanceSum
    part2 findTotalSimilaritySum
}
