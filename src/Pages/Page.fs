module LmuRaces.Pages.Page

open Fable.Core.JS
open Feliz
open ElmishLand
open LmuRaces.Shared
open LmuRaces.Pages
open RaceEvent
open Thoth.Json
open Thoth.Fetch
open Feliz.Shadcn
open Tier

// TODO list:
// sidebar menu avec le détail des courses
// loader
// Pouvoir changer le theme.
// Avoir son propre backend
// Championnat

type Model = {
    Events: RaceEvent list
    SelectedTier: Tier
    IsLoading: bool
}

type Msg =
    | LayoutMsg of Layout.Msg
    | EventsFetched of RaceEvent list
    | TierChanged of string

let fetchEvents (apiUrl: string) : Promise<RaceEvent list> =
    let customResolver =
        Extra.empty |> Extra.withCustom Decoders.tierEncoder Decoders.tierDecoder

    Fetch.get ($"{apiUrl}/race/events", caseStrategy = CamelCase, extra = customResolver)

let init (_shared: SharedModel) =
    {
        Events = []
        IsLoading = true
        SelectedTier = All
    },
    Command.ofPromise fetchEvents _shared.ApiUrl Msg.EventsFetched

let update (msg: Msg) (model: Model) =
    match msg with
    | LayoutMsg _ -> model, Command.none
    | EventsFetched events ->
        {
            model with
                Events = events
                IsLoading = false
        },
        Command.none
    | TierChanged tier ->
        {
            model with
                SelectedTier = tier |> Tier.FromString
        },
        Command.none

let loadingComponent () = Html.text "Loading..."

let tierSelector (dispatch: Msg -> unit) =
    Html.div [
        prop.className "m-3"
        prop.children [
            Shadcn.select [
                select.defaultValue "all"
                select.onValueChange (fun value -> dispatch (TierChanged value))
                prop.children [
                    Shadcn.selectTrigger [
                        Shadcn.selectValue [ prop.onChange (fun value -> dispatch (Msg.TierChanged value)) ]
                    ]
                    Shadcn.selectContent [
                        Shadcn.selectItem [ prop.value "all"; prop.text "All" ]
                        Shadcn.selectItem [ prop.value "beginner"; prop.text "Beginner" ]
                        Shadcn.selectItem [ prop.value "intermediate"; prop.text "Intermediate" ]
                        Shadcn.selectItem [ prop.value "advanced"; prop.text "Advanced" ]
                    ]
                ]
            ]
        ]
    ]

let raceEventsComponent (model: Model) (dispatch: Msg -> unit) =
    let events =
        model.Events
        |> List.filter (fun event ->
            match model.SelectedTier with
            | All -> true
            | selected -> event.Tier = selected)
        |> List.collect (fun event -> event.Schedules |> List.map (fun schedule -> (schedule, event)))
        |> List.sortBy fst

    Html.div [
        prop.children [
            tierSelector dispatch
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
                                Shadcn.tableCell [
                                    prop.children [
                                        Html.div event.Title
                                        Shadcn.badge [ prop.text (event.Tier |> string); badge.variant.destructive ]
                                    ]
                                ]
                                Shadcn.tableCell event.Track
                                Shadcn.tableCell event.Duration
                            ])
                ]
            ]
        ]
    ]

let view (_model: Model) (_dispatch: Msg -> unit) =
    if _model.IsLoading then
        loadingComponent ()
    else
        raceEventsComponent _model _dispatch

let page (_shared: SharedModel) (_route: HomeRoute) =
    Page.from (fun _ -> init _shared) update view () LayoutMsg
