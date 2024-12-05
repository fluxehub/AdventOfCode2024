#nowarn 0025

open Common.Runner
open FSharp.FGL
open FSharpPlus

let buildGraph edges =
    // Have to append unused labels to make the library happy
    let vertices =
        edges |> List.unzip ||> (@) |> List.distinct |> List.map (fun v -> v, ())

    Graph.empty
    |> Vertices.addMany vertices
    |> Directed.Edges.addMany (edges |> List.map (fun (a, b) -> a, b, ()))

let isSortedWith compare l = List.sortWith compare l = l

let compareEdge graph a b =
    if graph |> Directed.Edges.contains a b then -1 else 1

aoc {
    day 5

    mapInput (fun input ->
        let [ edges; paths ] =
            input
            |> String.split [ "\n\n" ]
            |> Seq.map (String.split [ "\n" ] >> List.ofSeq)
            |> List.ofSeq

        let graph =
            edges
            |> List.map (String.split [ "|" ] >> List.ofSeq >> (fun [ a; b ] -> int a, int b))
            |> buildGraph

        let paths = paths |> List.map (String.split [ "," ] >> List.ofSeq >> List.map int)

        graph, paths)

    part1 (fun (g, p) ->
        p
        |> List.filter (isSortedWith (compareEdge g))
        |> List.sumBy (fun l -> l[List.length l / 2]))

    part2 (fun (g, p) ->
        p
        |> List.filter (isSortedWith (compareEdge g) >> not)
        |> List.map (List.sortWith (compareEdge g))
        |> List.sumBy (fun l -> l[List.length l / 2]))
}
