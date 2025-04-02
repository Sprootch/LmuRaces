module LmuRaces.Pages.Page

open System
open Fable.Core.JS
open Feliz
open ElmishLand
open LmuRaces.Shared
open LmuRaces.Pages
open Thoth.Json
open Thoth.Fetch
open Feliz.Shadcn

type RaceEvent = {
    Title: string
    Tier: string
    Track: string
    Duration: string
    Schedules: DateTimeOffset List
}

type Model = {
    Events: RaceEvent list
    IsLoading: bool
}

type Msg =
    | LayoutMsg of Layout.Msg
    | EventsFetched of RaceEvent list

let fetchEvents (apiUrl: string) : Promise<RaceEvent list> =
    Fetch.get ($"{apiUrl}/race/events", caseStrategy = CamelCase)

let init (_shared: SharedModel) =
    { Events = []; IsLoading = true }, Command.ofPromise fetchEvents _shared.ApiUrl Msg.EventsFetched

let update (msg: Msg) (model: Model) =
    match msg with
    | LayoutMsg _ -> model, Command.none
    | EventsFetched events -> { Events = events; IsLoading = false }, Command.none

let loadingComponent () = Shadcn.button [ prop.text "Loading..." ]

let raceEventsComponent (events: RaceEvent list) =
    let events =
        events
        |> List.collect (fun event -> event.Schedules |> List.map (fun schedule -> (schedule, event)))
        |> List.sortBy fst

    Shadcn.table [
        Shadcn.tableCaption "test"
        Shadcn.tableHeader [
            Shadcn.tableRow [
                Shadcn.tableHead "Time"
                Shadcn.tableHead "Title"
            ]
        ]
        Shadcn.tableBody [
            Shadcn.tableRow [
                Shadcn.tableCell "A"
                Shadcn.tableCell "B"
            ]
        ]
    ]

let view (_model: Model) (_dispatch: Msg -> unit) =
    if _model.IsLoading then
        loadingComponent ()
    else
        raceEventsComponent _model.Events

let page (_shared: SharedModel) (_route: HomeRoute) =
    Page.from (fun _ -> init _shared) update view () LayoutMsg
