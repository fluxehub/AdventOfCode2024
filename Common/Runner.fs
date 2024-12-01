namespace Common

open FsHttp
open FSharpPlus

type AocDay<'i> =
    { Day: int
      InputTransformer: string -> 'i
      Parts: (string -> Unit) list }

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
              InputTransformer = id
              Parts = [ (fun _ -> ()); (fun _ -> ()) ] }

        [<CustomOperation("inputTransformer")>]
        member _.InputTransformer(day, processor) =
            { Day = day.Day
              InputTransformer = processor
              Parts = day.Parts } // Recreate because we need to change the type of AocDay

        [<CustomOperation("part")>]
        member _.Part(day, part, solution) =
            { day with
                Parts =
                    day.Parts
                    |> List.setAt (part - 1) (day.InputTransformer >> solution >> printPart part) }

        member _.Run(day) =
            let input = getDayInput day.Day

            day.Parts |> List.iter (fun part -> part input)

    let aoc = AocDayBuilder()
