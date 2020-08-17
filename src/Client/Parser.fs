namespace Cars

open Model
open System
open Cars
open Cars.Car

module Parser =

    let parseOrigin (origin : String) : String =
        match origin with
        |"" -> ""
        |"1.0000" -> "USA"
        |"2.0000" -> "Europe"
        |"3.0000" -> "Asia"
        | _ -> "Other"

    let isValid (car : Car) : bool =
        not (
            car.brand              = ""            ||
            car.name               = ""            ||
            car.mpg                |> Double.IsNaN ||
            car.cylinders          |> Double.IsNaN ||
            car.engineDisplacement |> Double.IsNaN ||
            car.horsepower         |> Double.IsNaN ||
            car.weight             |> Double.IsNaN ||
            car.acceleration       |> Double.IsNaN ||
            car.modelYear          |> Double.IsNaN ||
            car.origin             = ""
        )

    let parse (data : string) : list<Car> =
        failwith ""

