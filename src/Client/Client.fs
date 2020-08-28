module Client

open System
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Thoth.Fetch
open Fulma

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | LoadCsv
    | ChangeSorting
    | InitialCountLoaded of string
    //| SetHoverText of string
    | SelectCars of Set<Guid> * int * int
    | Deselect

let initialCounter () = Fetch.fetchAs<unit, string> "/api/init"

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =

    let loadCountCmd =
        Cmd.OfPromise.perform initialCounter () InitialCountLoaded

    Model.initialModel, loadCountCmd




// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.rawData, msg with
    | _, SelectCars (t,x,y) ->
        { currentModel with hoveredItems = t; positionX = x; positionY = y; }, Cmd.none       
    | _, Deselect ->
        { currentModel with hoveredItems = Set.empty}, Cmd.none
    | Some data, LoadCsv ->

        printfn "loading csv"
        data |> Data.loadCsv currentModel, Cmd.none
       
    | Some _, ChangeSorting ->
        let updatetSortmode =
            match currentModel.sortmode with
            | Unsorted -> Model.sorSortedbydes
            | Sortedbydes -> Sortedbyasce
            | Sortedbyasce -> Sortedbybrandorigin
            | Sortedbybrandorigin -> SortedbybrandoriginAsc
            | SortedbybrandoriginAsc -> Unsorted
        {
            currentModel with
                sortmode                   = updatetSortmode
                groupedCarsbybrandbyorigin = Data.Data.sortedCarsByBrandAndOrigin currentModel.cars updatetSortmode
                groupedCarsbybrand         = Data.Data.sortedCarsByBrand currentModel.cars updatetSortmode
        }, Cmd.none
    | _, InitialCountLoaded data ->                        
        data |> Data.Data.loadCsv currentModel, Cmd.none
    | _ -> currentModel, Cmd.none

let safeComponents =
    let components =
        span [ ]
           [ a [ Href "https://github.com/SAFE-Stack/SAFE-template" ]
               [ str "SAFE  "
                 str Version.template ]
             str ", "
             a [ Href "https://saturnframework.github.io" ] [ str "Saturn" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ]
           ]

    span [ ]
        [ str "Version "
          strong [ ] [ str Version.app ]
          str " powered by: "
          components ]

let show = function
    | { rawData = None   } -> "Loading..."
    | _ -> "loaded"

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]


//        <svg width="100" height="100">
//        <circle cx="50" cy="50" r="40" stroke="green" stroke-width="4" fill="yellow" />
//</svg>

let view (model : Model) (dispatch : Msg -> unit) =

    div []
        [
            Navbar.navbar [ Navbar.Color IsPrimary ] [
                Navbar.Item.div [ ]
                    [ Heading.h2 [ ]
                        [ str "Melihs und Sofies Daten-Visualiser!" ]
                    ]
                ]

            // let input =
            //     model.hoveredCarId
            //     |> Option.map (string)
            //     |> Option.defaultValue ("")

            let carMap =
                model.cars
                |> List.map (fun car -> (car.id, car))
                |> Map.ofList

            //Visualization.hoverDetail (carMap) (model)

            div [ Style [ Top model.positionY; Left model.positionX; Position PositionOptions.Absolute; ZIndex 999999;]] [ Visualization.hoverDetail (carMap) (model)]

            let height = 1100
            // let cy = 50
            let width = 2000

            let rangeLphundertkm = model.rangeLphundertkm

            // let isHovered (car : Car) =
            //     match model.hoveredItems with
            //     | Some hCar when hCar = car.id -> true
            //     | _ -> false

            let isHovered (car : Car) =
                model.hoveredItems.Contains (car.id)

            let isHoveredRect (cars : list<Car>) : bool * bool=
                let guids = cars |> List.map (fun car -> car.id)
                let currentSet = Set.ofList guids
                let isHovered = model.hoveredItems = currentSet
                let isPartly = model.hoveredItems |> Set.isSuperset currentSet
                isHovered, isPartly


            //let circles (rangeX : Domain) (rangeY : Domain) : list<ReactElement> =
            //    model.cars
            //    |> List.map (fun car ->
            //        Cars.Visualization.circle
            //            car
            //            (fun car -> car.mpg)
            //            (rangeX)
            //            (fun car -> car.horsepower)
            //            (rangeY)
            //            540
            //            500
            //            0.0
            //            (isHovered car)
            //            (fun evt -> dispatch (SelectCars (Set.ofList[car.id],(evt.pageX |> int),(evt.pageY |> int))))
            //    )
            
            let getCarCol car = 
                match car.origin with
                | USA    -> "#1f78b4"
                | Europe -> "#33a02c"
                | Asia   -> "#e31a1c"
                |  _     -> "#ffc800"

            let dim = (Vec2i.create 520 500)

            let circlesMpgHp =
                Visualization.ScatterPlot.drawCircles
                    dim
                    model.rangeCy
                    model.rangeHp
                    (fun (car : Car) -> car.cylinders)
                    (fun (car : Car) -> car.horsepower)
                    getCarCol
                    isHovered
                    (fun car evt -> dispatch (SelectCars (Set.ofList[car.id],(evt.pageX |> int),(evt.pageY |> int))))
                    model.cars

            let circlesLpHp =
                Visualization.ScatterPlot.drawCircles
                    dim
                    model.rangeLphundertkm
                    model.rangeHp
                    (fun (car : Car) -> car.lphundertkm)
                    (fun (car : Car) -> car.horsepower)
                    getCarCol
                    isHovered
                    (fun car evt -> dispatch (SelectCars (Set.ofList[car.id],(evt.pageX |> int),(evt.pageY |> int))))
                    model.cars

            //let circles1 (rangeX : Domain) (rangeY : Domain) : list<ReactElement> =
            //    model.cars
            //    |> List.map (fun car ->
            //        Cars.Visualization.circle
            //            car
            //            (fun car -> car.lphundertkm)
            //            (rangeX)
            //            (fun car -> car.horsepower)
            //            (rangeY)
            //            540
            //            500
            //            580.0
            //            (isHovered car)
            //            (fun evt -> dispatch (SelectCars (Set.ofList[car.id],(evt.pageX |> int),(evt.pageY |> int))))
            //    )

            

            let rects =
                match model.sortmode with
                | Unsorted
                | Sortedbydes
                | Sortedbyasce ->
                    let count = model.groupedCarsbybrand |> List.map(fun (a,b)  -> (b.Length))
                    let max = count |> tryMax
                    match max with
                    |None -> []
                    | _ ->
                        let newMax = count |> List.max
                        model.groupedCarsbybrand
                        |> List.mapi (fun i (brand,cars) ->
                            let hovered = cars |> List.map (fun car -> car.id)
                            let origin = model.originLookup |> Map.find brand
                            Cars.Visualization.rect
                                cars
                                (List.length count)
                                newMax
                                i
                                1080
                                500
                                590
                                (isHoveredRect cars)
                                origin
                                (fun evt -> dispatch (SelectCars (Set.ofList hovered,(evt.pageX |> int),(evt.pageY |> int))))
                            )
                | _ ->
                    let ncount = model.groupedCarsbybrandbyorigin |> List.map (fun (o, brandGroups) ->

                        let brandHisto = brandGroups |> List.map(fun (a,b)  -> (b.Length))
                        brandHisto
                    )
                    let count = ncount |> List.concat
                    let max = count |> tryMax
                    match max with
                    |None -> []
                    | _ ->
                        let newMax = count |> List.max
                        let brandgroups = model.groupedCarsbybrandbyorigin |> List.map (fun (o, brandGroups) -> brandGroups) |> List.concat
                        brandgroups |> List.mapi (fun i (brand,cars) ->
                            let hovered = cars |> List.map (fun car -> car.id)
                            let origin = model.originLookup |> Map.find brand
                            Cars.Visualization.rect
                                cars
                                (List.length count)
                                newMax
                                i
                                1080
                                500
                                500
                                (isHoveredRect cars)
                                origin
                                (fun evt -> dispatch (SelectCars (Set.ofList hovered,(evt.pageX |> int),(evt.pageY |> int))))
                            )           

            let lineX = line[X1 25; Y1 500; X2 525; Y2 500; Style [Stroke "black"]] []
            let lineY = line[X1 25; Y1 500; X2 25; Y2 0; Style [Stroke "black"]] []

            let circleLine = g [] circlesMpgHp
            let fcircleLine = g [SVGAttr.Transform "translate(525 0) scale (1 1)" ] circlesLpHp

            let lineX1 = line[X1 580; Y1 500; X2 1080; Y2 500; Style [Stroke "black"]] []
            let lineY1 = line[X1 580; Y1 500; X2 580; Y2 0; Style [Stroke "black"]] []

            //let circleLine1 = lineX1 :: circlesLpHp
            //let fcircleLine1 = lineY1 :: circleLine1

            //let x = 1200
            //let y = 50

            let textScatterplot=
                [
                    text[X 110; Y 520; SVGAttr.FontSize 20][
                        tspan[][str "Miles per Gallon (min : 9 - max : 47)"]
                    ]
                    text[X 670; Y 520; SVGAttr.FontSize 20;][
                        tspan[][str "liters per 100 km (min : 5 - max : 26)"]
                    ]
                    text[X 25; Y 500; SVGAttr.FontSize 20; SVGAttr.Transform "rotate(270,25,500) translate(115 -5)  scale(1 1)"][
                        tspan[][str "Horsepower (min : 46 - max : 230)"]
                    ]
                    text[X 580; Y 500; SVGAttr.FontSize 20; SVGAttr.Transform "rotate(270,580,500) translate(115 -5)  scale(1 1)"][
                        tspan[][str "Kilowatt (min : 34 - max : 172)"]
                    ]
                ]

            let textLegende =
                let x = 1200
                let y = 50
                [
                    text [X x; Y y; SVGAttr.FontSize 30][
                        tspan[X x; Dy 40][str "USA"]
                        tspan[X x; Dy 40][str "Europe"]
                        tspan[X x; Dy 40][str "Asia"]
                        tspan[X x; Dy 40][str "Other"]
                        tspan[X x; Dy 100][str "  Refresh"]
                        ]
                    circle [Cx (x-20); Cy (y+30); R "10"; SVGAttr.FillOpacity 0.8; SVGAttr.Fill "#1f78b4"][]
                    circle [Cx (x-20); Cy (y+70); R "10"; SVGAttr.FillOpacity 0.8; SVGAttr.Fill "#33a02c"][]
                    circle [Cx (x-20); Cy (y+110); R "10"; SVGAttr.FillOpacity 0.8; SVGAttr.Fill "#e31a1c"][]
                    circle [Cx (x-20); Cy (y+150); R "10"; SVGAttr.FillOpacity 0.8; SVGAttr.Fill "#ffc800"][]
                    circle [Cx (x-20); Cy (y+250); R "20"; SVGAttr.FillOpacity 0.8; SVGAttr.Fill "#888888"; OnMouseOver  (fun evt -> dispatch (SelectCars (Set.empty,(evt.pageX |> int),(evt.pageY |> int))))][]
                ]

            let svgContent = [[lineX];[lineY];[circleLine;fcircleLine];[lineX1];[lineY1];rects;textLegende; textScatterplot] |> List.concat

            Container.container [] [
                svg [SVGAttr.Width width; SVGAttr.Height height] svgContent
            ]

            Container.container [] [
                Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str ("Press buttons to manipulate counter: " + show model) ] ]
                Columns.columns []
                    [ Column.column [] [ button (sprintf "Sort (%A)" (model.sortmode.ToString())) (fun _ -> dispatch ChangeSorting) ]
                      Column.column [] [ button "LoadCsv" (fun _ -> dispatch LoadCsv) ]
                    ]                           

                let footer =
                    model.footer
                    |> List.map (fun x -> td[Style [Padding "10px"; BackgroundColor "#888888"; Color "#ffffff"]][x |> str])

                let header =
                    model.attributes
                    |> List.map (fun k -> th[Style [Padding "10px"; Color "#585858"]][str k])                

                let tableRows =
                    model.cars
                    |> List.mapi (fun i car ->
                        let c = Cars.Car.stringify car
                        Cars.Visualization.carToRow i c (isHovered car) (fun evt -> dispatch (SelectCars (Set.ofList[car.id],(evt.pageX |> int),(evt.pageY |> int))))
                    )

                table [] [
                    thead [] [
                        tr [] header
                    ]
                    tbody [] tableRows
                    tfoot [] [
                        tr [] footer
                    ]
                ]           
            ]

            Footer.footer [] [
                Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [
                    safeComponents
                ]
            ]
        ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
