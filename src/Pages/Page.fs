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

// TODO list:
// filtre sur tier
// loader
// sidebar menu avec le détail des courses
// Pouvoir changer le theme.
// Avoir son propre backend
// Championnat

type Tier =
    | Beginner
    | Intermediate
    | Advanced

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
    | TierChanged of string

let fetchEvents (apiUrl: string) : Promise<RaceEvent list> =
    Fetch.get ($"{apiUrl}/race/events", caseStrategy = CamelCase)

let init (_shared: SharedModel) =
    { Events = []; IsLoading = true }, Command.ofPromise fetchEvents _shared.ApiUrl Msg.EventsFetched

let update (msg: Msg) (model: Model) =
    match msg with
    | LayoutMsg _ -> model, Command.none
    | EventsFetched events ->
        { Events = events; IsLoading = false }, Command.none
    | TierChanged tier ->
        {
            model with
                Events = model.Events |> List.filter (fun e -> e.Tier = tier)
        },
        Command.none


let loadingComponent () = Html.text "Loading..."

let raceEventsComponent (events: RaceEvent list) (dispatch: Msg -> unit) =
    let events =
        events
        |> List.collect (fun event -> event.Schedules |> List.map (fun schedule -> (schedule, event)))
        |> List.sortBy fst

    Shadcn.table [
        Shadcn.tableHeader [
            Shadcn.tableRow [
                Shadcn.tableHead "Time"
                Shadcn.tableHead "Title"
                Shadcn.tableHead "Track"
                Shadcn.tableHead "Duration"
            ]
        ]
        Shadcn.tableBody [
            yield!
                events
                |> List.map (fun (dt, event) ->
                    Shadcn.tableRow [
                        Shadcn.tableCell $"""{dt.ToLocalTime().ToString("HH:mm")}"""
                        Shadcn.tableCell [ prop.children [ Html.div event.Title; Shadcn.badge [ prop.text event.Tier ; badge.variant.destructive] ] ]
                        Shadcn.tableCell event.Track
                        Shadcn.tableCell event.Duration
                    ])
        ]
    ]

let view (_model: Model) (_dispatch: Msg -> unit) =
    if _model.IsLoading then
        loadingComponent ()
    else
        raceEventsComponent _model.Events _dispatch

let page (_shared: SharedModel) (_route: HomeRoute) =
    Page.from (fun _ -> init _shared) update view () LayoutMsg
