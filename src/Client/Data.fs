namespace Client

module Data =

    let sortedCarsByBrand cars sortMode =
        let groupedByBrand = cars |> List.groupBy (fun (car : Car) -> car.brand)
        match sortMode with
        | Unsorted -> groupedByBrand
        | Sortedbyasce -> groupedByBrand |> List.sortBy (fun (brand,cars) ->  cars.Length)
        | Sortedbydes -> groupedByBrand |> List.sortByDescending (fun (brand,cars) ->  cars.Length)
        | _ -> []
    
    let sortedCarsByBrandAndOrigin cars sortMode =
        match sortMode with
        | Sortedbybrandorigin ->
            let originGroupedSorted = cars |> List.groupBy (fun car -> car.origin) |> List.sortByDescending (fun (orgin, cars) ->  cars.Length)
            let sortedCars =
                originGroupedSorted
                |> List.map (fun (origin, originCars) ->
                    let sortedBrands =
                        originCars
                        |> List.groupBy (fun car -> car.brand)
                        |> List.sortByDescending (fun (brand, brandCars) -> brandCars.Length)
                    origin, sortedBrands)
    
            sortedCars
        | SortedbybrandoriginAsc->
            let originGroupedSorted = cars |> List.groupBy (fun car -> car.origin) |> List.sortBy (fun (orgin, cars) ->  cars.Length)
            let sortedCars =
                originGroupedSorted
                |> List.map (fun (origin, originCars) ->
                    let sortedBrands =
                        originCars
                        |> List.groupBy (fun car -> car.brand)
                        |> List.sortBy (fun (brand, brandCars) -> brandCars.Length)
                    origin, sortedBrands)
            sortedCars
        | _ -> []   

    let calcAverageCars (x: list<Car>) =

        let count = x |> List.length|> float

        let sumCar =
            x
            |> List.fold (fun state car ->
                let newstate =
                    {
                        state with
                            mpg                = state.mpg + car.mpg
                            lphundertkm        = state.lphundertkm + car.lphundertkm
                            cylinders          = state.cylinders + car.cylinders
                            engineDisplacement = state.engineDisplacement + car.engineDisplacement
                            horsepower         = state.horsepower + car.horsepower
                            kw                 = state.kw + car.kw
                            weight             = state.weight + car.weight
                            acceleration       = state.acceleration + car.acceleration
                            modelYear          = state.modelYear + car.modelYear
                            brand = car.brand
                            origin = car.origin
                            name = "-"
                    }
                newstate) Car.empty
        
        let avgCar =
            {
                sumCar with
                    mpg                = sumCar.mpg          / count
                    lphundertkm        = sumCar.lphundertkm  / count
                    cylinders          = sumCar.cylinders    / count
                    engineDisplacement = sumCar.engineDisplacement / count
                    horsepower         = sumCar.horsepower   / count
                    kw                 = sumCar.kw           / count
                    weight             = sumCar.weight       / count
                    acceleration       = sumCar.acceleration / count
                    modelYear          = sumCar.modelYear    / count
            }
        
        avgCar

    let loadCsv model data : Model =
        let cars, header = Parser.parse data
        
        let header =
            let l,r = List.splitAt 3 header
            let tv = l @ ["L/100km"] @ r
        
            let x,y = List.splitAt 7 tv
            x @ ["KW"] @ y
        
        let rangeMpg =
            cars |> computeRange (fun car -> car.mpg)
        let rangelphundertkm =
            cars |> computeRange (fun car -> car.lphundertkm)
        let rangeHp =
            cars |> computeRange (fun car -> car.horsepower)
        let rangeCy =
            cars |> computeRange (fun car -> car.cylinders)
        
        let originLookup = Model.createOriginLookup(cars)                
        let avgCar = calcAverageCars(cars)
        
        let footer = [
            "Average:";
            "";
            sprintf "%.1f" (avgCar.mpg);
            sprintf "%.1f" (avgCar.lphundertkm);
            sprintf "%.1f" (avgCar.cylinders);
            sprintf "%.1f" (avgCar.engineDisplacement);
            sprintf "%.1f" (avgCar.horsepower);
            sprintf "%.1f" (avgCar.kw);
            sprintf "%.1f" (avgCar.weight);
            sprintf "%.1f" (avgCar.acceleration);
            sprintf "%.1f" (avgCar.modelYear );
            "n.a."
        ]                
                
        {
            model with
                cars                        = cars
                attributes                  = header
                groupedCarsbybrandbyorigin  = sortedCarsByBrandAndOrigin cars model.sortmode
                groupedCarsbybrand          = sortedCarsByBrand cars model.sortmode
                rangeMpg                    = rangeMpg
                rangeLphundertkm            = rangelphundertkm
                rangeHp                     = rangeHp
                rangeCy                     = rangeCy
                originLookup                = originLookup
                rawData                     = Some data
                footer                      = footer
        }   