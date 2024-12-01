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

    inputTransformer (fun input ->
        input
        |> String.split [ "\n" ]
        |> Seq.fold
            (fun (leftList, rightList) line ->
                let values = String.split [ "   " ] line |> Seq.map int |> Seq.toList
                values[0] :: leftList, values[1] :: rightList)
            ([], []))

    part 1 findTotalDistanceSum
    part 2 findTotalSimilaritySum
}
