﻿open Common

// I originally found a smart non-brute force method but I don't like the code
// Keeping it here anyway because solving it this way caused me great pain
// and I need recognition for my sacrifice

// let isSafeDifferences differences =
//     List.forall (fun v -> sign v = sign (List.head differences) && abs v > 0 && abs v <= 3) differences
//
// let removeRecord index differences =
//     if index = 0 then
//         List.tail differences
//     elif index = List.length differences then
//         differences |> List.deleteAt (index - 1)
//     else
//         differences
//         |> List.updateAt index (differences[index - 1] + differences[index])
//         |> List.deleteAt (index - 1)
//
// let canRemove differences predicate =
//     let matches = differences |> List.map predicate
//     let firstUnsafe = List.findIndex not matches
//
//     isSafeDifferences (removeRecord firstUnsafe differences)
//     || isSafeDifferences (removeRecord (firstUnsafe + 1) differences)
//
// let countSafeWithDampener reports =
//     reports
//     |> List.map findDifferences
//     |> List.filter (fun d ->
//         isSafeDifferences d
//         || canRemove d (fun x -> -3 <= x && x < 0) // Can't reuse predicate because of edge case
//         || canRemove d (fun x -> 0 < x && x <= 3))
//     |> List.length

let isSafe (report: int list) =
    let differences = List.pairwise report |> List.map (fun (a, b) -> b - a)

    List.forall (fun v -> sign v = sign (List.head differences) && abs v > 0 && abs v <= 3) differences

let countSafeWithDampenerBrute reports =
    reports
    |> List.filter (fun report ->
        isSafe report
        || report
           |> List.mapi (fun i _ -> report |> List.removeAt i |> isSafe)
           |> List.contains true)
    |> List.length

aoc {
    day 2

    mapLine (String.splitList " " >> List.map int)

    part1 (List.filter isSafe >> List.length)
    part2 countSafeWithDampenerBrute
}
