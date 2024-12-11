open Common

type Stones = Stones of Map<uint64, uint64>

module Stones =
    let private digitLength n = int (floor (log10 (float n)) + 1.0)

    let private split at n =
        let d = pown 10UL at
        (n / d, n % d)

    let private addToCount stone count (Stones stones) =
        let current = stones |> Map.tryFind stone |> Option.defaultValue 0UL
        Stones(stones |> Map.add stone (current + count))

    let ofList =
        List.countBy id
        >> List.map (fun (k, v) -> uint64 k, uint64 v)
        >> Map.ofList
        >> Stones

    let blink (Stones stones) =
        let rec buildNewStones newStones stones =
            match stones with
            | [] -> newStones
            | (stone, count) :: rest ->
                match (stone, digitLength stone) with
                | 0UL, _ -> buildNewStones (newStones |> addToCount 1UL count) rest
                | s, l when l % 2 = 0 ->
                    let s1, s2 = split (l / 2) s
                    buildNewStones (newStones |> addToCount s1 count |> addToCount s2 count) rest
                | s, _ -> buildNewStones (newStones |> addToCount (s * 2024UL) count) rest

        buildNewStones (Stones Map.empty) (Map.toList stones)

    let count (Stones stones) = stones |> Map.values |> Seq.sum

let rec runBlinks count stones =
    if count = 0 then
        stones
    else
        let newStones = Stones.blink stones
        runBlinks (count - 1) newStones

aoc {
    day 11

    mapInput (String.splitList " " >> List.map uint64 >> Stones.ofList)

    part1 (runBlinks 25 >> Stones.count)
    part2 (runBlinks 75 >> Stones.count)
}
