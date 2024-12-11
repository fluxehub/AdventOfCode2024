module Day06_LoopTimeMonitor

open System.Diagnostics
open Common
open FSharpPlus

type Guard =
    { Character: char; Position: int * int }

module Guard =
    let rotate guard =
        match guard.Character with
        | '^' -> { guard with Character = '>' }
        | '>' -> { guard with Character = 'v' }
        | 'v' -> { guard with Character = '<' }
        | '<' -> { guard with Character = '^' }
        | c -> failwith $"Invalid guard character: {c}"

    let tryMove (map: char array2d) guard =
        let w, h = Array2D.length1 map, Array2D.length2 map
        let x, y = guard.Position

        let newX, newY =
            match guard.Character with
            | '^' -> x, y - 1
            | '>' -> x + 1, y
            | 'v' -> x, y + 1
            | '<' -> x - 1, y
            | c -> failwith $"Invalid guard character: {c}"

        if newX < 0 || newX >= h || newY < 0 || newY >= w then None
        else if map[newY, newX] = '#' then Some(rotate guard)
        else Some { guard with Position = (newX, newY) }

type State =
    { Map: char array2d
      Guard: Guard
      Visited: Set<int * int> }

module State =
    let private tryStep state =
        match state.Guard |> Guard.tryMove state.Map with
        | None -> None
        | Some guard ->
            Some
                { state with
                    Guard = guard
                    Visited = state.Visited |> Set.add guard.Position }

    let run state =
        let rec loop state =
            match tryStep state with
            | None -> state
            | Some state -> loop state

        loop state

    let isLoop state =
        // This solution is here purely because it is the funniest way to solve the problem
        let stopwatch = Stopwatch.StartNew()

        let rec loop state =
            if stopwatch.ElapsedMilliseconds > 100 then // Works on my machine
                true
            else
                match tryStep state with
                | None -> false
                | Some state -> loop state

        loop state

let findGuardPosition inputMap =
    let rec loop map y =
        match List.head map |> String.tryFindIndex ((=) '^') with
        | Some x -> x, y
        | None -> loop (List.tail map) (y + 1)

    loop inputMap 0

let findVisitedPositions = State.run >> _.Visited

let run =
    aoc {
        day 6

        mapLines (fun lines ->
            let guardPosition = findGuardPosition lines

            { Map = lines |> List.map String.toList |> array2D
              Guard =
                { Character = '^'
                  Position = guardPosition }
              Visited = set [ guardPosition ] })

        part1 (fun state -> state |> State.run |> _.Visited |> Set.count)

        part2 (fun state ->
            state
            |> findVisitedPositions
            |> Array.ofSeq
            |> Array.Parallel.filter (fun (x, y) ->
                let newMap = Array2D.copy state.Map
                newMap[y, x] <- '#'
                State.isLoop { state with Map = newMap })
            |> Array.length)
    }
