namespace Cars

open Fable.React
open Fable.React.Props
open Model
open Cars

module Visualization =

    let circle (car: Car) (x : Car -> float) (rangeX : Domain) (y : Cars.Car -> float) (rangeY : Domain) width height offsetX isHovered (dispatch) : ReactElement =
        let cx = (((x car - rangeX.minimum) / rangeX.size) * (float width)) + offsetX
        let cy = ((y car - rangeY.minimum) / rangeY.size) * (float height)

        let strokeStyle =

            if isHovered then
                SVGAttr.StrokeWidth "2"
            else
                SVGAttr.StrokeWidth "0"

        match car.origin with
        | USA    -> circle [Cx cx; Cy (float height-cy); R "5"; SVGAttr.Stroke "black"; strokeStyle; SVGAttr.FillOpacity 0.4; SVGAttr.Fill "#1f78b4"; OnMouseOver   dispatch][]
        | Europe -> circle [Cx cx; Cy (float height-cy); R "5"; SVGAttr.Stroke "black"; strokeStyle; SVGAttr.FillOpacity 0.4; SVGAttr.Fill "#33a02c"; OnMouseOver  dispatch][]
        | Asia   -> circle [Cx cx; Cy (float height-cy); R "5"; SVGAttr.Stroke "black"; strokeStyle; SVGAttr.FillOpacity 0.4; SVGAttr.Fill "#e31a1c"; OnMouseOver    dispatch][]
        |  _     -> circle [Cx cx; Cy (float height-cy); R "5"; SVGAttr.Stroke "black"; strokeStyle; SVGAttr.FillOpacity 0.4; SVGAttr.Fill "#ffc800"; OnMouseOver dispatch][]

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

    //let circles (input : list<Car>) : list<ReactElement> = failwith ""

