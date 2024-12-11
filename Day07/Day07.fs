#nowarn 0025

open Common

// I don't understand how this works and trust me I tried to
let allComb l1 l2 =
    List.foldBack (fun x acc -> l1 |> List.collect (fun y -> acc |> List.map (fun rest -> (y, x) :: rest))) l2 [ [] ]

let canEvaluateToTarget operators (target, start :: rest) =
    let evaluateList = List.fold (fun acc (op, value) -> op acc value) start

    allComb operators rest |> List.map evaluateList |> List.contains target

let (++) a b = string a + string b |> int64

aoc {
    day 7

    mapLine (fun line ->
        let [ target; values ] = String.splitList ":" line
        int64 target, values |> String.splitList " " |> List.tail |> List.map int64)

    part1 (List.filter (canEvaluateToTarget [ (+); (*) ]) >> List.map fst >> List.sum)
    part2 (List.filter (canEvaluateToTarget [ (+); (*); (++) ]) >> List.map fst >> List.sum)
}
