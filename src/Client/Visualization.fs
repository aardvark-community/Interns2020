namespace Cars

open Fable.React
open Fable.React.Props
open Model
open Cars

module Visualization =

    let circle (car: Car) (x : Car -> float) (rangeX : Domain) (y : Cars.Car -> float) (rangeY : Domain) width height (dispatch) : ReactElement =
        let cx = ((x car - rangeX.minimum) / rangeX.size) * (float width)
        let cy = ((y car - rangeY.minimum) / rangeY.size) * (float height)

        //printf "%A %A" cx c
        match car.origin with
        |"1.0000" ->  circle [Cx cx; Cy (float height-cy); R "4"; SVGAttr.FillOpacity 0.3; SVGAttr.Stroke "Blue"; OnMouseOver   dispatch][]
        |"2.0000" ->  circle [Cx cx; Cy (float height-cy); R "4"; SVGAttr.FillOpacity 0.3; SVGAttr.Stroke "Green"; OnMouseOver  dispatch][]
        |"3.0000" ->  circle [Cx cx; Cy (float height-cy); R "4"; SVGAttr.FillOpacity 0.3; SVGAttr.Stroke "Red"; OnMouseOver    dispatch][]
        | _       ->  circle [Cx cx; Cy (float height-cy); R "4"; SVGAttr.FillOpacity 0.3; SVGAttr.Stroke "Yellow"; OnMouseOver dispatch][]

    let carToRow i (c : list<string>) (dispatch : Browser.Types.MouseEvent -> unit) : ReactElement =
        let tds =
            c |> List.map (fun x -> td[Style [Padding "10px"]; OnMouseOver dispatch][str x])

        tr [ if i%2=0 then Style [BackgroundColor "#cccccc"] else Style [BackgroundColor "#eeeeee"]] tds

    //let circles (input : list<Car>) : list<ReactElement> = failwith ""

