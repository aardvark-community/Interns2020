namespace Cars

open Fable.React
open Fable.React.Props
open Model
open Cars

module Visualization =

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
                    mpg                = sumCar.mpg / count
                    lphundertkm        = sumCar.lphundertkm / count
                    cylinders          = sumCar.cylinders / count
                    engineDisplacement = sumCar.engineDisplacement / count
                    horsepower         = sumCar.horsepower / count
                    kw                 = sumCar.kw / count
                    weight             = sumCar.weight / count
                    acceleration       = sumCar.acceleration / count
                    modelYear          = sumCar.modelYear / count
            }

        avgCar


    let circle (car: Car) (x : Car -> float) (rangeX : Domain) (y : Cars.Car -> float) (rangeY : Domain) width height offsetX isHovered (dispatch) : ReactElement =
        let offset = 25.0
        let cx = (((x car - rangeX.minimum) / rangeX.size) * (float width)) + offsetX + offset
        let cy = ((y car - rangeY.minimum) / rangeY.size) * (float height)

        let strokeStyle =

            if isHovered then
                SVGAttr.StrokeWidth "2"
            else
                SVGAttr.StrokeWidth "0"

        match car.origin with
        | USA    -> circle [Cx cx; Cy (float height-cy); R "4"; SVGAttr.Stroke "black"; strokeStyle; SVGAttr.FillOpacity 0.4; SVGAttr.Fill "#1f78b4"; OnMouseOver   dispatch][]
        | Europe -> circle [Cx cx; Cy (float height-cy); R "4"; SVGAttr.Stroke "black"; strokeStyle; SVGAttr.FillOpacity 0.4; SVGAttr.Fill "#33a02c"; OnMouseOver  dispatch][]
        | Asia   -> circle [Cx cx; Cy (float height-cy); R "4"; SVGAttr.Stroke "black"; strokeStyle; SVGAttr.FillOpacity 0.4; SVGAttr.Fill "#e31a1c"; OnMouseOver    dispatch][]
        |  _     -> circle [Cx cx; Cy (float height-cy); R "4"; SVGAttr.Stroke "black"; strokeStyle; SVGAttr.FillOpacity 0.4; SVGAttr.Fill "#ffc800"; OnMouseOver dispatch][]

    let rect (input : list<Car>) (count : int) (max : int) (index : int) (width : int) (height : int) (offsetY : int) (isHovered : bool * bool) (origin : Origin) (dispatch) : ReactElement =

        let x = (width/count)
        let offset = x * index
        let cars = input |> List.length
        let y = (height/max) * cars


        let rectStyle : list<IProp> =

            let backgroundColor =
                match origin with
                | USA    -> SVGAttr.Fill "#1f78b4"
                | Europe -> SVGAttr.Fill "#33a02c"
                | Asia   -> SVGAttr.Fill "#e31a1c"
                | _      -> SVGAttr.Fill "Grey"

            let (hovered, partly) = isHovered
            match hovered, partly with
            |true, _ -> [SVGAttr.FillOpacity 0.7; backgroundColor]
            |false, true -> [SVGAttr.StrokeWidth "2"; SVGAttr.Stroke "black"; backgroundColor]
            |false, false -> [backgroundColor]





        rect ([X offset; Y ((height-y) + offsetY); SVGAttr.Width x; SVGAttr.Height y; OnMouseOver dispatch] @ rectStyle) []




    let carToRow i (c : list<string>) isHovered (dispatch : Browser.Types.MouseEvent -> unit) : ReactElement =
        let tds =
            c |> List.map (fun x -> td[Style [Padding "10px"]; OnMouseOver dispatch][str x])

        let rowStyle =

            if isHovered then
                Style [BackgroundColor "#777777"; Color "#ffffff"]
            else
                if i%2=0 then Style [BackgroundColor "#cccccc"] else Style [BackgroundColor "#eeeeee"]

        tr [rowStyle] tds

    let createDetailrow (header : string) (value : string) (i : int) : ReactElement =
        let rowstyle =  if i%2=0 then Style [BackgroundColor "#cccccc"] else Style [BackgroundColor "#eeeeee"]
        tr[rowstyle][
            th[][str (sprintf "%s:" header)]
            td[][str value]
        ]

    let creatDetailNumeric (header : string) (value : float) (i : int) : ReactElement =
        let styledValue = sprintf "%.1f" value
        createDetailrow header styledValue i

    let createDetailcontent car =
        [
            createDetailrow("brand")(car.brand.ToString())0
            createDetailrow("Name")(car.name.ToString())1
            creatDetailNumeric("MpG")(car.mpg)2
            creatDetailNumeric("L/100km")(car.lphundertkm)3
            creatDetailNumeric("Cylinders")(car.cylinders)4
            creatDetailNumeric("Engine displacement")(car.engineDisplacement)5
            creatDetailNumeric("Horsepower")(car.horsepower)6
            creatDetailNumeric("KW")(car.kw)7
            creatDetailNumeric("Vehicle weight")(car.weight)8
            creatDetailNumeric("Acceleration")(car.acceleration)9
            creatDetailNumeric("Model year")(car.modelYear)10
            createDetailrow("Origin")(car.origin.ToString())11
        ]

    let hoverDetail carMap model=
        let setCount = model.hoveredItems |> Set.count
        match setCount with
        |0 -> table[][]
        |1 ->
            let carid = model.hoveredItems |> Set.toList |> List.head
            let car = carMap |> Map.find carid

            table[ Style [ Top model.positionY; Left model.positionX;]] (createDetailcontent car)
        |_ ->
            let car = model.hoveredItems |> Set.toList |> List.head
            let brand = (carMap |> Map.find car).brand
            let result = brand + " " + string setCount
            //let list = model.hoveredItems.tolist
            //let mpg = model.hoveredItems |> List.map (fun x -> x.mpg)
            let hoverdCars =
                model.hoveredItems
                |> Set.map (fun carid -> carMap |> Map.find carid)

            let avgCar =
                hoverdCars
                |> Set.toList
                |> calcAverageCars

            let count = hoverdCars |> Set.count |> float

            table[ Style [ Top model.positionY; Left model.positionX;]] ([creatDetailNumeric "Anzahl" count 1]@(createDetailcontent avgCar))



    //let circles (input : list<Car>) : list<ReactElement> = failwith ""

