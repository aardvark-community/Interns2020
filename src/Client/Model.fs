namespace Cars

open System
open Shared

type Vec2i =
    {
        x : int
        y : int
    }

module Vec2i =
    let create x y = { x = x ; y = y }
    let (+) l r = { x = l.x + r.x; y = l.y + r.y }
    let blurg() =
        let a = create 5 2
        let b = create 2 4

        a + b

type Origin =
| USA
| Europe
| Asia
| Other

type Sortmode =
    |Unsorted
    |Sortedbydes
    |Sortedbyasce
    |Sortedbybrandorigin
    |SortedbybrandoriginAsc

type Car = {
    id                 : System.Guid
    brand              : string
    name               : string
    mpg                : float
    lphundertkm        : float
    cylinders          : float
    engineDisplacement : float
    horsepower         : float //int
    kw                 : float
    weight             : float
    acceleration       : float
    modelYear          : float //int
    origin             : Origin
}




// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = {
    rawData             : option<string>
    cars                : list<Car>
    attributes          : list<String>
    footer              : list<String>
    attributes2         : list<String>
    groupedCarsbybrandbyorigin : list<Origin * list<string * list<Car>>>
    groupedCarsbybrand        : list<string * list<Car>>
    rangeMpg            : Domain
    rangeCy             : Domain
    rangeHp             : Domain
    rangeEd             : Domain
    rangeLphundertkm    : Domain
    rangekw             : Domain
    sortmode            : Sortmode
    //hoveredCarId        : option<Guid>
    hoveredItems        : Set<Guid>
    positionX           : int
    positionY           : int
    originLookup        : Map<string,Origin>

}

module Car =

    let stringify (car : Car) : list<string> =
        [
            car.brand
            car.name
            string car.mpg
            sprintf "%.1f" car.lphundertkm
            string car.cylinders
            string car.engineDisplacement
            string car.horsepower
            sprintf "%.1f" car.kw
            string car.weight
            string car.acceleration
            string car.modelYear
            car.origin.ToString()
        ]

    let empty : Car =
        {
            id                 = System.Guid.NewGuid()
            brand              =""
            name               =""
            mpg                = 0.0
            lphundertkm        = 0.0
            cylinders          = 0.0
            engineDisplacement = 0.0
            horsepower         = 0.0
            kw                 = 0.0
            weight             = 0.0
            acceleration       = 0.0
            modelYear          = 0.0
            origin             = Origin.Other
        }

module Model =




    let initialModel =
        {
            rawData = None
            cars = []
            attributes = []
            footer = []
            attributes2 = []
            groupedCarsbybrandbyorigin = []
            groupedCarsbybrand = []
            rangeMpg =
                {
                    maximum = 0.0
                    minimum = 0.0
                    size = 0.0
                }
            rangeHp =
                {
                    maximum = 0.0
                    minimum = 0.0
                    size = 0.0
                }
            rangeCy =
                {
                    maximum = 0.0
                    minimum = 0.0
                    size = 0.0
                }
            rangeEd =
                {
                    maximum = 0.0
                    minimum = 0.0
                    size = 0.0
                }
            rangeLphundertkm =
                {
                    maximum = 0.0
                    minimum = 0.0
                    size = 0.0
                }
            rangekw =
                {
                    maximum = 0.0
                    minimum = 0.0
                    size = 0.0
                }
            //hoveredCarId = None
            positionX = 0
            positionY = 0
            hoveredItems = Set.empty
            originLookup = Map.empty
            sortmode = Unsorted
        }


    let CreateOriginLookup (cars : list<Car>) : (Map<string, Origin>)=
        let gCars = cars |> List.groupBy (fun x -> x.brand)
        let flookupt = gCars |> List.map (fun (a,b) -> a, b.Head.origin)


        flookupt |> Map.ofList

    let originLookup = CreateOriginLookup
