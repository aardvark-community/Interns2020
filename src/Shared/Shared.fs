namespace Client

type Counter = { Value : int }

[<AutoOpen>]
module Shared =
    type Domain = {
        minimum : float
        maximum : float
        size    : float
    }


    let tryMax xs = if Seq.isEmpty xs then None else Seq.max xs |> Some

    let computeRange (getAttribute : 'a -> float)  data =
        let attribute = data |> List.map getAttribute
        let max = attribute |> List.max
        let min = attribute |> List.min
        let range = {
            minimum = min//  - float 1
            maximum = max//  + float 1
            size = max - min
        }
        range