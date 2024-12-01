namespace Common

open FsHttp
open FSharpPlus

type AocDay<'i> =
    { Day: int
      InputTransformer: string -> 'i
      Solutions: (string -> Unit) list }

module Runner =
    let private readSessionToken = System.IO.File.ReadAllText "session"

    let private getDayInput day =
        http {
            GET $"https://adventofcode.com/2024/day/{day}/input"
            Cookie "session" readSessionToken
        }
        |> Request.send
        |> Response.toString None
        |> String.trimEnd "\n" // Strip trailing newline

    let private printAnswer part answer = printfn $"Part {part}: {answer}"

    let private setSolution part inputTransformer solution partList =
        partList
        |> List.setAt (part - 1) (inputTransformer >> solution >> printAnswer part)

    type AocDayBuilder() =
        member _.Yield(()) = ()

        [<CustomOperation("day")>]
        member _.Day((), day) =
            { Day = day
              InputTransformer = id
              Solutions = [ (fun _ -> ()); (fun _ -> ()) ] }

        [<CustomOperation("inputTransformer")>]
        member _.InputTransformer(day, processor) =
            { Day = day.Day
              InputTransformer = processor
              Solutions = day.Solutions } // Recreate because we need to change the type of AocDay

        [<CustomOperation("part1")>]
        member _.Part1(day, solution) =
            { day with
                Solutions = day.Solutions |> setSolution 1 day.InputTransformer solution }

        [<CustomOperation("part2")>]
        member _.Part2(day, solution) =
            { day with
                Solutions = day.Solutions |> setSolution 2 day.InputTransformer solution }

        member _.Run(day) =
            let input = getDayInput day.Day

            day.Solutions |> List.iter (fun part -> part input)

    let aoc = AocDayBuilder()
