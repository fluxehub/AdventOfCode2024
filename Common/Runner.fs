namespace Common

open System.IO
open FsHttp
open FSharpPlus
open Microsoft.FSharp.Core

type AocDay<'i> =
    { Day: int
      Input: string option
      InputMapper: string -> 'i
      Solutions: (string -> Unit) list }

module String =
    // Returns a list, not a seq
    let splitList separator =
        String.split [ separator ] >> Seq.toList

[<AutoOpen>]
module Runner =
    let private readSessionToken = System.IO.File.ReadAllText "session"

    let private downloadDayInput day =
        http {
            GET $"https://adventofcode.com/2024/day/{day}/input"
            Cookie "session" readSessionToken
        }
        |> Request.send
        |> Response.toString None
        |> String.trimEnd "\n" // Strip trailing newline

    let private getDayInput day =
        // Read input file exists, otherwise download it
        if File.Exists $"day{day}.txt" then
            File.ReadAllText $"day{day}.txt"
        else
            let input = downloadDayInput day
            File.WriteAllText($"day{day}.txt", input)
            input

    let private printAnswer part answer = printfn $"Part {part}: {answer}"

    let private setSolution part inputMapper solution partList =
        partList
        |> List.updateAt (part - 1) (inputMapper >> solution >> printAnswer part)

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
                input |> String.splitList "\n" |> List.map mapper

            { Day = day.Day
              Input = day.Input
              InputMapper = inputMapper
              Solutions = day.Solutions } // Recreate because we need to change the type of AocDay

        [<CustomOperation("mapLines")>]
        member _.LinesMapper(day, mapper) =
            let inputMapper input =
                input |> String.splitList "\n" |> mapper

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
