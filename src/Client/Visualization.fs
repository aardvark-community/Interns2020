namespace Cars

open Fable.React
open Fable.React.Props
open Model
open Cars

module Visualization =

    let circle (car: Car) (x : Car -> float) (rangeX : Domain) (y : Cars.Car -> float) (rangeY : Domain) width height (dispatch) : ReactElement =
        let cx = ((x car - rangeX.minimum) / rangeX.size) * (float width)
        let cy = ((y car - rangeY.minimum) / rangeY.size) * (float height)
        
        match car.origin with
        | USA    -> circle [Cx cx; Cy (float height-cy); R "4"; SVGAttr.FillOpacity 0.3; SVGAttr.Fill "Blue"; OnMouseOver   dispatch][]
        | Europe -> circle [Cx cx; Cy (float height-cy); R "4"; SVGAttr.FillOpacity 0.3; SVGAttr.Fill "Green"; OnMouseOver  dispatch][]
        | Asia   -> circle [Cx cx; Cy (float height-cy); R "4"; SVGAttr.FillOpacity 0.3; SVGAttr.Fill "Red"; OnMouseOver    dispatch][]
        |  _     -> circle [Cx cx; Cy (float height-cy); R "4"; SVGAttr.FillOpacity 0.3; SVGAttr.Fill "Grey"; OnMouseOver dispatch][]

    let carToRow i (c : list<string>) (dispatch : Browser.Types.MouseEvent -> unit) : ReactElement =
        let tds =
            c |> List.map (fun x -> td[Style [Padding "10px"]; OnMouseOver dispatch][str x])

        tr [ if i%2=0 then Style [BackgroundColor "#cccccc"] else Style [BackgroundColor "#eeeeee"]] tds

    //let circles (input : list<Car>) : list<ReactElement> = failwith ""

