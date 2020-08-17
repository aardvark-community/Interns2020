namespace Cars

open System

type Origin =
| USA
| Europe
| Asia
| Other

type Car = {
    brand              : string
    name               : string
    mpg                : float
    cylinders          : float
    engineDisplacement : float
    horsepower         : float //int
    weight             : float
    acceleration       : float
    modelYear          : float //int
    origin             : Origin
}

type Domain = {
    minimum : float
    maximum : float
    size    : float
}

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = {
    rawData     : option<string>
    cars        : list<Car>
    attributes  : list<String>
    footer      : list<String>
    attributes2 : list<String>
    groupedCars : list<list<Car>>
    rangeMpg    : Domain
    rangeCy     : Domain
    rangeHp     : Domain
    rangeEd     : Domain
    hoverText   : string
}

module Car =
    let stringify (car : Car) : list<string> =
        [
            car.brand
            car.name
            string car.mpg
            string car.cylinders
            string car.engineDisplacement
            string car.horsepower
            string car.weight
            string car.acceleration
            string car.modelYear
            car.origin.ToString()
        ]

module Model = 
    
    
    

    let initialModel =
        {
            rawData = None
            cars = []
            attributes = []
            footer = []
            attributes2 = []
            groupedCars = []
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
            hoverText = "lala"
        }

