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
      Visited: Set<Guard> }

module State =
    let rec tryStep state =
        match state.Guard |> Guard.tryMove state.Map with
        | None -> None, false
        | Some guard ->
            if state.Visited |> Set.contains guard then
                Some state, true
            else
                Some
                    { state with
                        Guard = guard
                        Visited = state.Visited |> Set.add guard },
                false

    let run state =
        let rec loop state =
            match tryStep state with
            | None, _ -> state, false
            | Some state, true -> state, true
            | Some state, false -> loop state

        loop state

let findGuardPosition inputMap =
    let rec loop map y =
        match List.head map |> String.tryFindIndex ((=) '^') with
        | Some x -> x, y
        | None -> loop (List.tail map) (y + 1)

    loop inputMap 0

let findVisitedPositions = State.run >> fst >> _.Visited >> Set.map _.Position

aoc {
    day 6

    mapLines (fun lines ->
        let guard =
            { Character = '^'
              Position = findGuardPosition lines }

        { Map = lines |> List.map String.toList |> array2D
          Guard = guard
          Visited = set [ guard ] })

    part1 (findVisitedPositions >> Set.count)

    part2 (fun state ->
        state
        |> findVisitedPositions
        |> Set.toList
        |> List.filter (fun (x, y) -> state.Map[y, x] = '.')
        |> List.filter (fun (x, y) ->
            state.Map[y, x] <- '#'
            let _, isLoop = State.run state
            state.Map[y, x] <- '.'
            isLoop)
        |> List.length)
}
