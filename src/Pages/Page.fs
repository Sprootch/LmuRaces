module LmuRaces.Pages.Page

open System
open Fable.Core.JS
open Feliz
open ElmishLand
open LmuRaces.Shared
open LmuRaces.Pages
open Thoth.Json
open Thoth.Fetch

type RaceEvent = {
    Title: string
    Tier: string
    Track: string
    Duration: string
    Schedules: DateTimeOffset List
}

type Model = { Events: RaceEvent list }

type Msg =
    | LayoutMsg of Layout.Msg
    | EventsFetched of RaceEvent list

let fetchEvents (apiUrl: string) : Promise<RaceEvent list> =
    Fetch.get ($"{apiUrl}/race/events", caseStrategy = CamelCase)

let init (_shared: SharedModel) =
    { Events = [] }, Command.ofPromise fetchEvents _shared.ApiUrl Msg.EventsFetched

let update (msg: Msg) (model: Model) =
    match msg with
    | LayoutMsg _ -> model, Command.none
    | EventsFetched events -> { Events = events }, Command.none

let view (_model: Model) (_dispatch: Msg -> unit) =
    let events =
        _model.Events
        |> List.collect (fun event ->
            event.Schedules |> List.map (fun schedule -> (schedule, event)))
        |> List.sortBy fst

    Html.ul [
        prop.children [
            yield!
                events
                |> List.map (fun (dt, item) ->
                    Html.li $"""{dt.ToLocalTime().ToString("HH:mm")} / {item.Title} / {item.Tier} / {item.Track} / {item.Duration}""")
        ]
    ]

let page (_shared: SharedModel) (_route: HomeRoute) =
    Page.from (fun _ -> init _shared) update view () LayoutMsg
