open Common
open FSharpPlus

type RegionStats = { Perimeter: int; Area: int }

let adjacentPlots (x, y) map =
    [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
    |> List.filter (fun (ax, ay) -> (map |> Array2D.isInBounds (ax, ay)) && map[y, x] = map[ay, ax])

let visitRegion p char visited map =
    let rec visitLoop toVisit stats visited =
        match toVisit with
        | [] -> (stats, visited)
        | p :: rest when visited |> Set.contains p -> visitLoop rest stats visited
        | p :: rest ->
            let adjacent = adjacentPlots p map

            let newStats =
                { stats with
                    Area = stats.Area + 1
                    Perimeter = stats.Perimeter + (4 - List.length adjacent) }

            visitLoop (adjacent @ rest) newStats (visited |> Set.add p)

    visitLoop [ p ] { Perimeter = 0; Area = 0 } visited

let visitAllRegions map x y c (regionStats, visited) =
    if visited |> Set.contains (x, y) then
        (regionStats, visited)
    else
        let stats, visited = visitRegion (x, y) c visited map
        (stats :: regionStats, visited)

aoc {
    day 12

    //     input
    //         "RRRRIICCFF
    // RRRRIICCCF
    // VVRRRCCFFF
    // VVRCCCJFFF
    // VVVVCJJCFE
    // VVIVCCJJEE
    // VVIIICJJEE
    // MIIIIIJJEE
    // MIIISIJEEE
    // MMMISSJEEE"

    mapLines (List.map String.toList >> array2D)

    part1 (fun map ->
        map
        |> Array2D.foldi (visitAllRegions map) ([], Set.empty)
        |> fst
        |> List.sumBy (fun s -> s.Area * s.Perimeter))
}
