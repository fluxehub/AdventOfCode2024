[<AutoOpen>]
module Common.Helpers

open FSharpPlus

module String =
    // Returns a list, not a seq
    let splitList separator =
        String.split [ separator ] >> Seq.toList

// We do this so much this year
module Array2D =
    let isInBounds (x, y) array =
        x >= 0 && x < Array2D.length2 array && y >= 0 && y < Array2D.length1 array

    let foldi folder state array =
        let w, h = Array2D.length2 array, Array2D.length1 array

        let rec loop x y state =
            if y = h then
                state
            else
                let state = folder x y array[y, x] state
                let y = if x = w - 1 then y + 1 else y
                let x = if x = w - 1 then 0 else x + 1
                loop x y state

        loop 0 0 state
