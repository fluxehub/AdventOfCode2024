open Common.Runner
open FSharpPlus

let safePredicate report =
    (List.forall ((>) 0) report || List.forall ((<) 0) report)
    && List.forall (abs >> (>=) 3) report

let removeRecord index differences =
    if index = 0 then
        List.tail differences
    elif index = List.length differences then
        differences |> List.deleteAt (index - 1)
    else
        differences
        |> List.updateAt index (differences[index - 1] + differences[index])
        |> List.deleteAt (index - 1)

let canRemove differences predicate =
    let matches = differences |> List.map predicate
    let firstUnsafe = List.findIndex not matches

    safePredicate (removeRecord firstUnsafe differences)
    || safePredicate (removeRecord (firstUnsafe + 1) differences)

let countSafeReports reports =
    reports
    |> List.map (fun report -> List.map2 (-) (List.take (List.length report - 1) report) (List.tail report))
    |> List.filter safePredicate
    |> List.length

let countSafeWithDampener reports =
    reports
    |> List.map (fun report -> List.map2 (-) (List.take (List.length report - 1) report) (List.tail report))
    |> List.filter (fun d ->
        safePredicate d
        || canRemove d (fun x -> -3 <= x && x < 0)
        || canRemove d (fun x -> 0 < x && x <= 3))
    |> List.length

aoc {
    day 2

    inputTransformer (fun input ->
        input
        |> String.split [ "\n" ]
        |> Seq.map (String.split [ " " ] >> Seq.map int >> Seq.toList)
        |> Seq.toList)

    part1 countSafeReports
    part2 countSafeWithDampener
}
