open Common.Runner

let splitInput (input: string) =
    input.Split("\n")
    |> Array.fold
        (fun (leftList, rightList) line ->
            let values = line.Split("   ")
            let left = int values[0]
            let right = int values[1]
            left :: leftList, right :: rightList)
        ([], [])

let findTotalDistanceSum (left, right) =
    let left = List.sort left
    let right = List.sort right

    List.zip left right |> List.map (fun (a, b) -> abs (a - b)) |> List.sum

let findTotalSimilaritySum (left, right) =
    let appearances = right |> List.countBy id |> Map.ofList

    left
    |> List.sumBy (fun v -> v * (appearances |> Map.tryFind v |> Option.defaultValue 0))

aoc {
    day 1
    part1 findTotalDistanceSum
    part2 findTotalSimilaritySum
}
|> run splitInput
