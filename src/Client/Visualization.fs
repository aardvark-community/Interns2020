namespace Cars

open Fable.React
open Fable.React.Props
open Model
open Cars
open Shared

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

    let circle (c : Vec2i) r col isHovered onhover : ReactElement =

        let strokeStyle =
            if isHovered then
                SVGAttr.StrokeWidth "2"
            else
                SVGAttr.StrokeWidth "0"
        circle [Cx c.x; Cy c.y; R r; SVGAttr.Stroke "black"; strokeStyle; SVGAttr.FillOpacity 0.4; SVGAttr.Fill col; OnMouseOver onhover][]


    let dataCircle
        (a : 'a)
        (x : 'a -> float)
        (rangeX : Domain)
        (y : 'a -> float)
        (rangeY : Domain)
        (col : 'a -> string)
        (size : Vec2i)
        offsetX
        isHovered onhover =
        
        let cx = int <| (((x a - rangeX.minimum) / rangeX.size) * (float size.x))
        let cy = int <| (((y a - rangeY.minimum) / rangeY.size) * (float size.y))
        let cy = size.y - cy

        let c = Vec2i.create cx cy
        let col = col a

        circle c "4" col isHovered onhover


    module ScatterPlot =
        let drawCircles dim (rangeX : Domain) (rangeY : Domain) getX getY getCol isHovered onHover data : list<ReactElement> =
            data
            |> List.map (fun d ->
                dataCircle
                    d
                    getX
                    rangeX
                    getY
                    rangeY
                    getCol
                    dim
                    0.0
                    (isHovered d)
                    (onHover d)//(fun evt -> dispatch (SelectCars (Set.ofList[car.id],(evt.pageX |> int),(evt.pageY |> int))))
            )



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

    let createDetailcontent car avg =
        [
            let average = if avg then "Ø" else ""

            createDetailrow("brand")(car.brand.ToString())0
            createDetailrow("Name")(car.name.ToString())1
            creatDetailNumeric("MpG " + average)(car.mpg)2
            creatDetailNumeric("L/100km " + average)(car.lphundertkm)3
            creatDetailNumeric("Cylinders " + average)(car.cylinders)4
            creatDetailNumeric("Engine displacement " + average)(car.engineDisplacement)5
            creatDetailNumeric("Horsepower " + average)(car.horsepower)6
            creatDetailNumeric("KW " + average)(car.kw)7
            creatDetailNumeric("Vehicle weight " + average)(car.weight)8
            creatDetailNumeric("Acceleration " + average)(car.acceleration)9
            creatDetailNumeric("Model year " + average)(car.modelYear)10
            createDetailrow("Origin")(car.origin.ToString())11
        ]

    let hoverDetail carMap model=
        let setCount = model.hoveredItems |> Set.count
        match setCount with
        |0 -> table[][]
        |1 ->
            let carid = model.hoveredItems |> Set.toList |> List.head
            let car = carMap |> Map.find carid

            table[ Style [ Top model.positionY; Left model.positionX;]] (createDetailcontent car false)
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

            table[ Style [ Top model.positionY; Left model.positionX;]] ([creatDetailNumeric "Anzahl" count 1]@(createDetailcontent avgCar true))

    //let circles (input : list<Car>) : list<ReactElement> = failwith ""

