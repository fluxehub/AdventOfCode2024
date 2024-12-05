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

let buildPath vertices =
    (vertices |> List.take (List.length vertices - 1), List.tail vertices)
    ||> List.zip

let isPathValid graph path =
    path |> List.forall (fun (a, b) -> Directed.Edges.contains a b graph)

let sumPathCenters =
    List.map (fun l -> l |> List.map fst |> List.item (List.length l / 2)) >> List.sum

let compareEdge graph a b =
    if graph |> Directed.Edges.contains a b then
        -1
    else
        1
    
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
        
        let pathEdges = List.map buildPath paths
        
        graph, paths, pathEdges)

    part1 (fun (g, _, p) ->
        p
        |> List.filter (isPathValid g)
        |> sumPathCenters)
    
    part2 (fun (g, p, pe) ->
        List.zip p pe
        |> List.filter (fun (_, pe) -> isPathValid g pe |> not)
        |> List.map fst
        |> List.map (List.sortWith (compareEdge g))
        |> List.sumBy (fun l -> l[l.Length / 2]))
}
