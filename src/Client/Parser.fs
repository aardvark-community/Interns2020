namespace Cars

open System
open Cars

module Parser =

    let num = 235.215
    let num1 = 0.7457

    let parseOrigin (origin : float) : Origin =
        match origin with
        |1.0 -> Origin.USA
        |2.0 -> Origin.Europe
        |3.0 -> Origin.Asia
        | _ -> Origin.Other

    let isValid (car : Car) : bool =
        not (
            car.brand              = ""            ||
            car.name               = ""            ||
            car.mpg                |> Double.IsNaN ||
            car.lphundertkm        |> Double.IsNaN ||
            car.cylinders          |> Double.IsNaN ||
            car.engineDisplacement |> Double.IsNaN ||
            car.horsepower         |> Double.IsNaN ||
            car.weight             |> Double.IsNaN ||
            car.acceleration       |> Double.IsNaN ||
            car.modelYear          |> Double.IsNaN
        )

    let tryParse d =
        match d with
        | "" -> nan
        |  _ -> d |> Double.Parse

    let parseRow i (row : string) : Car =

        printfn "splitting row"
        printfn "%d %A" i row
        let attr = row.Split(',')

        let carName = attr.[0].Trim()

        let index = carName.IndexOf(' ')

        let brand = carName.Substring(0, index)

        let name = carName.Substring(index)

        let output = {
            id                 = System.Guid.NewGuid()
            brand              = brand
            name               = name
            mpg                = attr.[1] |> tryParse
            lphundertkm        = num / (attr.[1] |> tryParse)
            cylinders          = attr.[2] |> tryParse
            engineDisplacement = attr.[3] |> tryParse
            horsepower         = attr.[4] |> tryParse //int
            kw                 = num1 * (attr.[4] |> tryParse)
            weight             = attr.[5] |> tryParse
            acceleration       = attr.[6] |> tryParse
            modelYear          = attr.[7] |> tryParse //int
            origin             = attr.[8] |> tryParse |> parseOrigin
        }

        output

    let parse (input:string) : list<Car> * list<string> =
        let rows = input.Split('\n') |> Array.toList

        match rows with
        | [] -> [],[]
        | head::tail ->
            printfn "parsing header"
            let header = head.Split(',') |> Array.toList
            let newHeader = "Brand" :: header

            let cars = tail |> List.mapi (fun i row -> parseRow i row)

            let carsFiltered =
                cars
                |> List.filter(isValid)
            carsFiltered, newHeader
