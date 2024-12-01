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

    let private setPart inputTransformer solution part partList =
        partList
        |> List.setAt (part - 1) (inputTransformer >> solution >> printPart part)

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

        [<CustomOperation("part1")>]
        member _.Part1(day, solution) =
            { day with
                Parts = day.Parts |> setPart day.InputTransformer solution 1 }

        [<CustomOperation("part2")>]
        member _.Part2(day, solution) =
            { day with
                Parts = day.Parts |> setPart day.InputTransformer solution 2 }

        member _.Run(day) =
            let input = getDayInput day.Day

            day.Parts |> List.iter (fun part -> part input)

    let aoc = AocDayBuilder()
