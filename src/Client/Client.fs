module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json

open Shared
open System

open Cars

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


let sortedCarsByBrand cars sortMode =
    let groupedByBrand = cars |> List.groupBy (fun car -> car.brand)
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



// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.rawData, msg with
    | _, SelectCars (t,x,y) ->
        { currentModel with hoveredItems = t; positionX = x; positionY = y; }, Cmd.none
       // { currentModel with  = i }, Cmd.none
       // { currentModel with positionY = s }, Cmd.none
    | _, Deselect ->
        { currentModel with hoveredItems = Set.empty}, Cmd.none
    | Some data, LoadCsv ->

        printfn "loading csv"
        let cars,header2 = Cars.Parser.parse data

        let header =
            let l,r = List.splitAt 3 header2
            let tv = l @ ["L/100km"] @ r

            let x,y = List.splitAt 7 tv
            x @ ["KW"] @ y


        let rangeMpg =
            let mpg = cars |> List.map (fun car -> car.mpg)
            let range = {
                minimum = (mpg |> List.min)//  - float 1
                maximum = (mpg |> List.max)//  + float 1
                size = (mpg |> List.max) - (mpg |> List.min)
            }
            range

        let rangeHp =
            let horsepower = cars |> List.map (fun car -> car.horsepower)
            let range = {
                minimum = (horsepower |> List.min)//  - float 1
                maximum = (horsepower |> List.max)//  + float 1
                size = (horsepower |> List.max) - (horsepower |> List.min)
            }
            range

        let rangelphundertkm =
            let lphundertkm = cars |> List.map (fun car -> car.lphundertkm)
            let range = {
                minimum = (lphundertkm |> List.min)//  - float 1
                maximum = (lphundertkm |> List.max)//  + float 1
                size = (lphundertkm |> List.max) - (lphundertkm |> List.min)
            }
            range

        let originLookup = Model.CreateOriginLookup(cars)
        let currentModel =
            {
                currentModel with
                    cars                = cars
                    attributes          = header
                    groupedCarsbybrandbyorigin         = sortedCarsByBrandAndOrigin cars currentModel.sortmode
                    groupedCarsbybrand         = sortedCarsByBrand cars currentModel.sortmode
                    rangeMpg            = rangeMpg
                    rangeLphundertkm    = rangelphundertkm
                    rangeHp             = rangeHp
                    originLookup        = originLookup
            }

       // currentModel, Cmd.none

        // let rangeCy =
        //    let cylinders = newCars |> List.map (fun car -> car.cylinders)
        //    let range = {
        //        minimum = (cylinders |> List.min)//  - float 1
        //        maximum = (cylinders |> List.max)//  + float 1
        //        size = (cylinders |> List.max) - (cylinders |> List.min)
        //    }
        //    range

        // let rangeEd =
        //    let cylinders = newCars |> List.map (fun car -> car.cylinders)
        //    let range = {
        //        minimum = (cylinders |> List.min)//  - float 1
        //        maximum = (cylinders |> List.max)//  + float 1
        //        size = (cylinders |> List.max) - (cylinders |> List.min)
        //    }
        //    range

        let avgCar = Cars.Visualization.calcAverageCars(cars)

        let foot = ["Average:"; ""; sprintf "%.1f" (avgCar.mpg); sprintf "%.1f" (avgCar.lphundertkm); sprintf "%.1f" (avgCar.cylinders); sprintf "%.1f" (avgCar.engineDisplacement); sprintf "%.1f" (avgCar.horsepower);  sprintf "%.1f" (avgCar.kw);sprintf "%.1f" (avgCar.weight); sprintf "%.1f" (avgCar.acceleration); sprintf "%.1f" (avgCar.modelYear ); "USA"]

        let asdf = {
                currentModel with
                    footer = foot;
                   }
        asdf, Cmd.none


        // let m = {
        //    currentModel with
        //        cars = cars;
        //        attributes = newHead;
        //        footer = foot;
        //        attributes2 = head2;
        //        groupedCars = groupedCars;
        //        rangeMpg = rangeMpg;
        //        rangeHp = rangeHp;
        //        rangeCy = rangeCy;
        //        rangeEd = rangeEd;
        // }
        // m, Cmd.none

    | Some counter, ChangeSorting ->
        let updatetSortmode =
            match currentModel.sortmode with
            | Unsorted -> Sortedbydes
            | Sortedbydes -> Sortedbyasce
            | Sortedbyasce -> Sortedbybrandorigin
            | Sortedbybrandorigin -> SortedbybrandoriginAsc
            | SortedbybrandoriginAsc -> Unsorted

        {currentModel with
            sortmode = updatetSortmode
            groupedCarsbybrandbyorigin         = sortedCarsByBrandAndOrigin currentModel.cars updatetSortmode
            groupedCarsbybrand         = sortedCarsByBrand currentModel.cars updatetSortmode
            }, Cmd.none
    | _, InitialCountLoaded data ->
        let nextModel = { Model.initialModel with rawData = Some data }

        printf "%s" data
        nextModel, Cmd.none
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

            let height = 1000
            // let cy = 50
            let width = 2000

            let rangeMpg = model.rangeMpg
            let rangeHp = model.rangeHp
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


            let circles (rangeX : Domain) (rangeY : Domain) : list<ReactElement> =
                model.cars
                |> List.map (fun car ->
                    Cars.Visualization.circle
                        car
                        (fun car -> car.mpg)
                        (rangeX)
                        (fun car -> car.horsepower)
                        (rangeY)
                        540
                        500
                        0.0
                        (isHovered car)
                        (fun evt -> dispatch (SelectCars (Set.ofList[car.id],(evt.pageX |> int),(evt.pageY |> int))))
                )

            let circles1 (rangeX : Domain) (rangeY : Domain) : list<ReactElement> =
                model.cars
                |> List.map (fun car ->
                    Cars.Visualization.circle
                        car
                        (fun car -> car.lphundertkm)
                        (rangeX)
                        (fun car -> car.horsepower)
                        (rangeY)
                        540
                        500
                        580.0
                        (isHovered car)
                        (fun evt -> dispatch (SelectCars (Set.ofList[car.id],(evt.pageX |> int),(evt.pageY |> int))))
                )

            let tryMax xs =
              if Seq.isEmpty xs
              then
                None
              else
                Seq.max xs |> Some

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
                                500
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

            // let rects =
            //     let count = cars |> List.map (List.length)
            //     let max = count |> tryMax
            //     match max with
            //     |None -> []
            //     | _ ->
            //         let newMax = count |> List.max
            //         model.groupedCars
            //         |> List.mapi (fun i (brand,cars) ->
            //             let hovered = carsi |> List.map (fun car -> car.id)
            //             let origin = model.originLookup |> Map.find brand
            //             Cars.Visualization.rect
            //                 cars
            //                 (List.length count)
            //                 newMax
            //                 i
            //                 1080
            //                 500
            //                 500
            //                 (isHoveredRect cars)
            //                 origin
            //                 (fun _ -> dispatch (SelectCars (Set.ofList hovered)))
            //             )

            let lineX = line[X1 25; Y1 500; X2 525; Y2 500; Style [Stroke "black"]] []
            let lineY = line[X1 25; Y1 500; X2 25; Y2 0; Style [Stroke "black"]] []

            let circleLine = lineX :: circles rangeMpg rangeHp
            let fcircleLine = lineY :: circleLine

            let lineX1 = line[X1 580; Y1 500; X2 1080; Y2 500; Style [Stroke "black"]] []
            let lineY1 = line[X1 580; Y1 500; X2 580; Y2 0; Style [Stroke "black"]] []

            let circleLine1 = lineX1 :: circles1 rangeLphundertkm rangeHp
            let fcircleLine1 = lineY1 :: circleLine1

            //let x = 1200
            //let y = 50

            let textScatterplot=
                [
                    text[X 300; Y 480; SVGAttr.FontSize 20][
                        tspan[X 175; Dy 40][str "Miles per Gallon"]
                    ]
                    text[X 300; Y 480; SVGAttr.FontSize 20;][
                        tspan[X 775; Dy 40][str "liters per 100 km"]
                    ]
                    text[X 25; Y 500; SVGAttr.FontSize 20; SVGAttr.Transform "rotate(270,25,500) translate(235 -5)  scale(1 1)"][
                        tspan[][str "Horsepower"]
                    ]
                    text[X 580; Y 500; SVGAttr.FontSize 20; SVGAttr.Transform "rotate(270,580,500) translate(235 -5)  scale(1 1)"][
                        tspan[][str "Kilowatt"]
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
                        ]
                    circle [Cx (x-20); Cy (y+30); R "10"; SVGAttr.FillOpacity 0.8; SVGAttr.Fill "#1f78b4"][]
                    circle [Cx (x-20); Cy (y+70); R "10"; SVGAttr.FillOpacity 0.8; SVGAttr.Fill "#33a02c"][]
                    circle [Cx (x-20); Cy (y+110); R "10"; SVGAttr.FillOpacity 0.8; SVGAttr.Fill "#e31a1c"][]
                    circle [Cx (x-20); Cy (y+150); R "10"; SVGAttr.FillOpacity 0.8; SVGAttr.Fill "#ffc800"][]
                ]

            let svgContent = [fcircleLine;fcircleLine1;rects;textLegende; textScatterplot] |> List.concat

            Container.container [] [
                svg [SVGAttr.Width width; SVGAttr.Height height] svgContent
            ]

            Container.container [] [
                Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str ("Press buttons to manipulate counter: " + show model) ] ]
                Columns.columns []
                    [ Column.column [] [ button (sprintf "Sort (%A)" (model.sortmode.ToString())) (fun _ -> dispatch ChangeSorting) ]
                      Column.column [] [ button "LoadCsv" (fun _ -> dispatch LoadCsv) ] ]

                let header2 =
                    model.attributes2
                    |> List.map (fun k -> th[Style [Padding "10px"; Color "#585858"]][str k])

                //let carNames =
                    //model.cars
                     //|> List.filter (fun c -> c.name.StartsWith("a"))
                    // |> List.sortBy (fun car -> car.name)
                    // |> List.map (fun car -> li[][str car.name])

                let footer =
                    model.footer
                    |> List.map (fun x -> td[Style [Padding "10px"; BackgroundColor "#888888"; Color "#ffffff"]][x |> str])

                let header =
                    model.attributes
                    |> List.map (fun k -> th[Style [Padding "10px"; Color "#585858"]][str k])

                // let carToUl (l : list<Car>) : ReactElement =
                //     let l2 = l |> List.map (fun car -> li [] [str car.name])
                //     td [Style [Padding "10px"; Color "#585858"]] [ul [] l2]

                // let cars2 =
                //     model.groupedCars
                //     |> List.map (carToUl)

                // let stringifiedCars =
                //     model.cars
                //     |> List.map(Cars.Car.stringify)

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


                // table [] [
                //     thead [] [
                //         tr [] header2
                //     ]
                //     tbody [] [
                //         tr [] cars2
                //     ]
                // ]
                // ul [] carNames
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
