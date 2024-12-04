namespace Common

open FsHttp
open FSharpPlus
open Microsoft.FSharp.Core

type AocDay<'i> =
    { Day: int
      Input: string option
      InputMapper: string -> 'i
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

    let private setSolution part inputMapper solution partList =
        partList |> List.setAt (part - 1) (inputMapper >> solution >> printAnswer part)

    type AocDayBuilder() =
        member _.Yield(()) = ()

        [<CustomOperation("day")>]
        member _.Day((), day) =
            { Day = day
              Input = None
              InputMapper = id
              Solutions = [ (fun _ -> ()); (fun _ -> ()) ] }

        [<CustomOperation("input")>]
        member _.Input(day, input) = { day with Input = Some input }

        [<CustomOperation("mapInput")>]
        member _.InputMapper(day, mapper) =
            { Day = day.Day
              Input = day.Input
              InputMapper = mapper
              Solutions = day.Solutions } // Recreate because we need to change the type of AocDay

        [<CustomOperation("mapLine")>]
        member _.LineMapper(day, mapper) =
            let inputMapper input =
                input |> String.split [ "\n" ] |> Seq.map mapper |> Seq.toList

            { Day = day.Day
              Input = day.Input
              InputMapper = inputMapper
              Solutions = day.Solutions } // Recreate because we need to change the type of AocDay

        [<CustomOperation("mapLines")>]
        member _.LinesMapper(day, mapper) =
            let inputMapper input =
                input |> String.split [ "\n" ] |> Seq.toList |> mapper

            { Day = day.Day
              Input = day.Input
              InputMapper = inputMapper
              Solutions = day.Solutions } // Recreate because we need to change the type of AocDay

        [<CustomOperation("part1")>]
        member _.Part1(day, solution) =
            { day with
                Solutions = day.Solutions |> setSolution 1 day.InputMapper solution }

        [<CustomOperation("part2")>]
        member _.Part2(day, solution) =
            { day with
                Solutions = day.Solutions |> setSolution 2 day.InputMapper solution }

        member _.Run(day) =
            let input = day.Input |> Option.defaultValue (getDayInput day.Day)

            day.Solutions |> List.iter (fun part -> part input)

    let aoc = AocDayBuilder()
