open Common

type Stones = Stones of Map<uint64, uint64>

module Stones =
    let private digitLength n = n |> float |> log10 |> int |> (+) 1

    let private split at n =
        let d = pown 10UL at
        (n / d, n % d)

    let private addStones stone count (Stones stones) =
        Stones(stones |> Map.change stone (Option.defaultValue 0UL >> (+) count >> Some))

    let private (|Zero|_|) (stone, count) =
        if stone = 0UL then Some(count) else None

    let private (|EvenDigits|_|) (stone, count) =
        let l = digitLength stone
        if l % 2 = 0 then Some(stone, l, count) else None

    let private (|Stone|) (stone, count) = stone, count

    let ofList =
        List.countBy id >> List.map (fun (k, v) -> k, uint64 v) >> Map.ofList >> Stones

    let blink (Stones stones) =
        let rec buildNewStones newStones stones =
            match stones with
            | [] -> newStones
            | Zero count :: rest -> buildNewStones (newStones |> addStones 1UL count) rest
            | EvenDigits(stone, l, count) :: rest ->
                let s1, s2 = split (l / 2) stone
                buildNewStones (newStones |> addStones s1 count |> addStones s2 count) rest
            | Stone(stone, count) :: rest -> buildNewStones (newStones |> addStones (stone * 2024UL) count) rest

        buildNewStones (Stones Map.empty) (Map.toList stones)

    let count (Stones stones) = stones |> Map.values |> Seq.sum

let rec runBlinks count stones =
    if count = 0 then
        stones
    else
        Stones.blink stones |> runBlinks (count - 1)

aoc {
    day 11

    mapInput (String.splitList " " >> List.map uint64 >> Stones.ofList)

    part1 (runBlinks 25 >> Stones.count)
    part2 (runBlinks 75 >> Stones.count)
}
