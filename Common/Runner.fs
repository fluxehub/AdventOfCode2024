namespace Common

open FsHttp
open FSharpPlus

type AocDay<'i> =
    { Day: int
      InputProcessor: string -> 'i
      Parts: (string -> Unit) option list }

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

    let private printPart part answer = printfn $"Part {part}: {answer}"

    type AocDayBuilder() =
        member _.Yield(()) = ()

        [<CustomOperation("day")>]
        member _.Day((), day) =
            { Day = day
              InputProcessor = id
              Parts = [ None; None ] }

        [<CustomOperation("inputProcessor")>]
        member _.InputProcessor(day, processor) =
            { Day = day.Day
              InputProcessor = processor
              Parts = day.Parts } // Recreate because we need to change the type of AocDay

        [<CustomOperation("part")>]
        member _.Part(day, part, solution) =
            { day with
                Parts = [ Some(day.InputProcessor >> solution >> printPart part); day.Parts[part - 1] ] }

        member _.Run(day) =
            let input = getDayInput day.Day

            day.Parts |> List.iter (Option.iter (fun part -> part input))

    let aoc = AocDayBuilder()
