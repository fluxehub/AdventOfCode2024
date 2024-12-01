namespace Common

open FsHttp

type Solution<'i, 'a> = 'i -> 'a

type AocDay<'i, 'a, 'b> =
    { Day: int
      Part1: Solution<'i, 'a> option
      Part2: Solution<'i, 'b> option }

module Runner =
    type AocDayBuilder() =
        member _.Yield(()) = ()

        [<CustomOperation("day")>]
        member _.Day((), day) =
            { Day = day
              Part1 = None
              Part2 = None }

        [<CustomOperation("part1")>]
        member _.Part1(day, solution) = { day with Part1 = Some solution }

        [<CustomOperation("part2")>]
        member _.Part2(day, solution) = { day with Part2 = Some solution }

    let aoc = AocDayBuilder()

    let private readSessionToken = System.IO.File.ReadAllText "session"

    let private getDayInput day =
        http {
            GET $"https://adventofcode.com/2024/day/{day}/input"
            Cookie "session" readSessionToken
        }
        |> Request.send
        |> Response.toString None
        |> _.TrimEnd('\n') // Strip trailing newline

    let private runSolution input solution =
        solution |> Option.map (fun solution -> solution input)

    let run inputProcessor day =
        let input = inputProcessor <| getDayInput day.Day

        runSolution input day.Part1
        |> Option.iter (fun answer -> (printfn $"Part 1: {answer}"))

        runSolution input day.Part2
        |> Option.iter (fun answer -> (printfn $"Part 2: {answer}"))
