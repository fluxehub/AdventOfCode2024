open Common
open FSharpPlus

let getAdjacentPoints (x, y, v) map =
    [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]
    |> List.filter (fun (x, y) ->
        x >= 0
        && y >= 0
        && x < Array2D.length1 map
        && y < Array2D.length2 map
        && map[y, x] - 1 = v)

let scoreTrailhead (px, py) (map: int array2d) =
    let rec search toVisit (map: int array2d) (visited: (int * int) Set) =
        match toVisit with
        | [] -> visited
        | (px, py) :: rest when (map[py, px] = 9) -> search rest map (visited |> Set.add (px, py))
        | (px, py) :: rest ->
            let v = map[py, px]

            search ((getAdjacentPoints (px, py, v) map) @ rest) map visited

    search [ (px, py) ] map Set.empty

let rateTrailhead (px, py) (map: int array2d) =
    let rec search toVisit (map: int array2d) currentPath (paths: (int * int) list Set) =
        match toVisit with
        | [] -> paths
        | (px, py) :: rest when (map[py, px] = 9) ->
            search rest map currentPath (paths |> Set.add ((px, py) :: currentPath))
        | (px, py) :: rest ->
            let v = map[py, px]

            search ((getAdjacentPoints (px, py, v) map) @ rest) map ((px, py) :: currentPath) paths

    search [ (px, py) ] map [] Set.empty

let findTrailheads map =
    map
    |> Array2D.mapi (fun y x c -> (x, y, c))
    |> Seq.cast<int * int * int>
    |> Seq.filter (fun (_, _, c) -> c = 0)

aoc {
    day 10

    mapLines (List.map (String.toList >> List.map (fun c -> int c - 48)) >> array2D)

    part1 (fun map ->
        map
        |> findTrailheads
        |> Seq.sumBy (fun (x, y, _) -> scoreTrailhead (x, y) map |> Set.count))

    part2 (fun map ->
        map
        |> findTrailheads
        |> Seq.sumBy (fun (x, y, _) -> rateTrailhead (x, y) map |> Set.count))
}
