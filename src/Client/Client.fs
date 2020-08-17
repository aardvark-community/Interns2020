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
    | Decrement
    | InitialCountLoaded of string
    | SetHoverText of string

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
    | _, SetHoverText t ->
        { currentModel with hoverText = t }, Cmd.none
    | Some data, LoadCsv ->

        printfn "loading csv"
        let cars,header = Cars.Parser.parse data        

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

        let currentModel =
            {
                currentModel with
                    cars       = cars
                    attributes = header
                    rangeMpg   = rangeMpg
                    rangeHp    = rangeHp
            }

        currentModel, Cmd.none
      
            //let rangeCy =
            //    let cylinders = newCars |> List.map (fun car -> car.cylinders)
            //    let range = {
            //        minimum = (cylinders |> List.min)//  - float 1
            //        maximum = (cylinders |> List.max)//  + float 1
            //        size = (cylinders |> List.max) - (cylinders |> List.min)
            //    }
            //    range

            //let rangeEd =
            //    let cylinders = newCars |> List.map (fun car -> car.cylinders)
            //    let range = {
            //        minimum = (cylinders |> List.min)//  - float 1
            //        maximum = (cylinders |> List.max)//  + float 1
            //        size = (cylinders |> List.max) - (cylinders |> List.min)
            //    }
            //    range

            //let gCars = newCars |> List.groupBy (fun car -> car.brand)

            //let head2 =
            //    gCars |> List.map (fst)

            //let groupedCars =
            //    gCars |> List.map (snd)

            //let carMpg =
            //    newCars
            //    |> List.map (fun car -> car.mpg)

            //let carCyl =
            //    newCars
            //    |> List.map (fun car -> car.cylinders)

            //let carEng =
            //    newCars
            //    |> List.map (fun car -> car.engineDisplacement)

            //let carHp =
            //    newCars
            //    |> List.map (fun car -> car.horsepower)

            //let carVw =
            //    newCars
            //    |> List.map (fun car -> car.weight)

            //let carAcc =
            //    newCars
            //    |> List.map (fun car -> car.acceleration)

            //let carMy =
            //    newCars
            //    |> List.map (fun car -> car.modelYear)

            //let numOfCars =
            //    newCars
            //    |> List.length

            //let avg (input : list<float>) : float =
            //    input |> List.fold (fun avg x -> avg + (x/(float numOfCars))) 0.0

            //let foot = ["Average:"; ""; sprintf "%.1f" (avg carMpg); sprintf "%.1f" (avg carCyl); sprintf "%.1f" (avg carEng); sprintf "%.1f" (avg carHp); sprintf "%.1f" (avg carVw); sprintf "%.1f" (avg carAcc); sprintf "%.1f" (avg carMy); ""]


            //let m = {
            //    currentModel with
            //        cars = cars;
            //        attributes = newHead;
            //        //footer = foot;
            //        //attributes2 = head2;
            //        //groupedCars = groupedCars;
            //        //rangeMpg = rangeMpg;
            //        //rangeHp = rangeHp;
            //        //rangeCy = rangeCy;
            //        //rangeEd = rangeEd;
            //}
            //m, Cmd.none

    | Some counter, Decrement ->
        currentModel, Cmd.none
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
        
            div [ Style [FontSize "20"; Top 0; Left 0; Position PositionOptions.Fixed]] [ str model.hoverText ]

            let height = 700
            let cy = 50
            let width = 1080
        
            let rangeMpg = model.rangeMpg
            let rangeHp = model.rangeHp                    
                    
            let circles (rangeX : Domain) (rangeY : Domain) : list<ReactElement> =
                model.cars
                |> List.map (fun car ->                
                    Cars.Visualization.circle
                        car
                        (fun car -> car.mpg)
                        (rangeX)
                        (fun car -> car.horsepower)
                        (rangeY)
                        width
                        height
                        (fun _ -> dispatch (SetHoverText car.name))
                )
        
            //let circles : list<ReactElement> = []
        
            let lineX = line[X1 0; Y1 700; X2 1080; Y2 700; Style [Stroke "black"]] []
            let lineY = line[X1 0; Y1 700; X2 0; Y2 0; Style [Stroke "black"]] []
        
            let circleLine = lineX :: circles rangeMpg rangeHp
            let fcircleLine = lineY :: circleLine
        
            Container.container [] [
                svg [SVGAttr.Width width; SVGAttr.Height height] fcircleLine
            ]
        
            Container.container [] [
                Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str ("Press buttons to manipulate counter: " + show model) ] ]
                Columns.columns []
                    [ Column.column [] [ button "-" (fun _ -> dispatch Decrement) ]
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
        
                let carToUl (l : list<Car>) : ReactElement =
                    let l2 = l |> List.map (fun car -> li [] [str car.name])
                    td [Style [Padding "10px"; Color "#585858"]] [ul [] l2]
        
                let cars2 =
                    model.groupedCars
                    |> List.map (carToUl)
        
                let stringifiedCars =
                    model.cars
                    |> List.map(fun c -> Cars.Car.stringify c)
        
                let tableRows =
                    stringifiedCars
                    |> List.mapi (fun i c ->
                        Cars.Visualization.carToRow i c (fun _ -> dispatch (SetHoverText "brrr"))
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
        
                table [] [
                    thead [] [
                        tr [] header2
                    ]
                    tbody [] [
                        tr [] cars2
                    ]
                ]
                //ul [] carNames
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
