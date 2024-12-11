#nowarn 0025

open Common
open FSharpPlus

type Id = int

type Block =
    | File of Id * int
    | Empty of int

module Block =
    let toOption =
        function
        | File(i, l) -> Some(i, l)
        | Empty _ -> None

let checksum disk =
    let rec loop sum pos disk =
        match disk with
        | [] -> sum
        | File(_, 0) :: disk -> loop sum pos disk
        | File(i, l) :: disk -> loop (sum + uint64 i * uint64 pos) (pos + 1) (File(i, l - 1) :: disk)
        | Empty l :: disk -> loop sum (pos + l) disk

    loop 0UL 0 disk

let compactPart1 disk =
    let files = disk |> List.choose Block.toOption

    let rec loop leftToAdd newDisk disk files =
        if leftToAdd = 1 then
            let i, l = List.head files
            List.rev (File(i, l) :: newDisk)
        else
            match disk, files with
            | File(i, l) :: rest, _ -> loop (leftToAdd - 1) (File(i, l) :: newDisk) rest files
            | Empty emptyLen :: diskRest, (i, fileLen) :: filesRest ->
                if emptyLen = fileLen then
                    loop (leftToAdd - 1) (File(i, fileLen) :: newDisk) diskRest filesRest
                else if emptyLen > fileLen then
                    loop (leftToAdd - 1) (File(i, fileLen) :: newDisk) (Empty(emptyLen - fileLen) :: diskRest) filesRest
                else
                    loop leftToAdd (File(i, emptyLen) :: newDisk) diskRest ((i, fileLen - emptyLen) :: filesRest)

    loop (List.length files) [] disk (List.rev files)

let compactPart2 disk =
    let rec tryMove newDisk disk file =
        match disk, file with
        | File(aId, aLength) :: rest, File(bId, _) ->
            if aId = bId then
                None
            else
                tryMove (File(aId, aLength) :: newDisk) rest file
        | Empty emptyLen :: rest, File(i, fileLen) ->
            if emptyLen = fileLen then
                Some((List.rev (file :: newDisk)) @ (List.replace [ file ] [ Empty(fileLen) ] rest))
            else if emptyLen > fileLen then
                Some(
                    (List.rev (Empty(emptyLen - fileLen) :: file :: newDisk))
                    @ (List.replace [ file ] [ Empty(fileLen) ] rest)
                )
            else
                tryMove (Empty(emptyLen) :: newDisk) rest file

    let rec loop disk diskRev =
        match diskRev with
        | [] -> disk
        | Empty _ :: rest -> loop disk rest
        | file :: rest ->
            match tryMove [] disk file with
            | None -> loop disk rest
            | Some disk -> loop disk rest

    loop disk (List.rev disk)

aoc {
    day 9

    mapInput (fun input ->
        let rec buildFileSystem (i, acc) layout =
            match layout with
            | [] -> acc
            | [ fileLen ] -> File(i, fileLen) :: acc
            | fileLen :: emptyLen :: rest -> File(i, fileLen) :: Empty emptyLen :: buildFileSystem (i + 1, acc) rest

        buildFileSystem (0, []) (String.toList input |> List.map (fun c -> int c - 48)))

    part1 (compactPart1 >> checksum)
    part2 (compactPart2 >> checksum)
}
