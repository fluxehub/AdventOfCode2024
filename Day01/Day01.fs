#nowarn 0025

open Common

let findTotalDistanceSum (left, right) =
    (List.sort left, List.sort right) ||> List.map2 (-) |> List.sumBy abs

let findTotalSimilaritySum (left, right) =
    let appearances = right |> List.countBy id |> Map.ofList

    left
    |> List.sumBy (fun v -> v * (appearances |> Map.tryFind v |> Option.defaultValue 0))

aoc {
    day 1

    mapLines (List.map (String.splitList "   " >> fun [ l; r ] -> int l, int r) >> List.unzip)

    part1 findTotalDistanceSum
    part2 findTotalSimilaritySum
}
