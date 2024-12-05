open System.Text.RegularExpressions
open Common
open FSharpPlus

let sumOfMul input =
    Regex.Matches(input, "mul\((\d+),(\d+)\)")
    |> Seq.sumBy (fun m -> int m.Groups[1].Value * int m.Groups[2].Value)

aoc {
    day 3

    part1 sumOfMul

    part2 (fun input ->
        Regex.Replace(String.replace "\n" "" input, "don't\(\).+?do\(\)|don't\(\).+", "")
        |> sumOfMul)
}
