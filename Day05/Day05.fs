#nowarn 0025

open Common

let isSortedWith compare l = List.sortWith compare l = l

let compareEdge edgeSet a b =
    if edgeSet |> Set.contains (a, b) then -1 else 1

let center l = l |> List.item (List.length l / 2)

aoc {
    day 5

    mapInput (fun input ->
        let [ edges; paths ] =
            input |> String.splitList "\n\n" |> List.map (String.splitList "\n")

        let edgeSet =
            edges
            |> List.map (String.splitList "|" >> (fun [ a; b ] -> int a, int b))
            |> Set.ofList

        let paths = paths |> List.map (String.splitList "," >> List.map int)

        edgeSet, paths)

    part1 (fun (es, p) -> p |> List.filter (isSortedWith (compareEdge es)) |> List.sumBy center)

    part2 (fun (es, p) ->
        p
        |> List.filter (isSortedWith (compareEdge es) >> not)
        |> List.map (List.sortWith (compareEdge es))
        |> List.sumBy center)
}
