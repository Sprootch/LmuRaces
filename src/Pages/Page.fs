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
// Avoir son propre backend
// Championnat

type Filter =
  | All
  | Track of string
  | Tier of Tier

type Model = {
  Events: RaceEvent list
  SelectedFilter: Filter
  IsLoading: bool
  ApiUrl: string
  Result: string
}

type Msg =
  | LayoutMsg of Layout.Msg
  | EventsFetched of RaceEvent list
  | FilterChanged of Filter
  | ApiFetched of string
  | Refresh

let fetchEvents (apiUrl: string) : Promise<RaceEvent list> =
  let customResolver =
    Extra.empty
    |> Extra.withCustom Decoders.tierEncoder Decoders.tierDecoder

  Fetch.get ($"{apiUrl}/race/events", caseStrategy = CamelCase, extra = customResolver)

let init (_shared: SharedModel) =
  {
    Events = []
    IsLoading = true
    SelectedFilter = All
    ApiUrl = _shared.ApiUrl
    Result = ""
  },
  Command.batch [
    // Command.ofPromise fetchapi _shared.ApiUrl Msg.ApiFetched
    Command.ofPromise fetchEvents _shared.ApiUrl Msg.EventsFetched
  ]

let update (msg: Msg) (model: Model) =
  match msg with
  | LayoutMsg _ -> model, Command.none
  | ApiFetched res -> { model with Result = res }, Command.none
  | Refresh -> { model with IsLoading = true }, Command.ofPromise fetchEvents model.ApiUrl Msg.EventsFetched
  | EventsFetched events -> { model with Events = events; IsLoading = false }, Command.none
  | FilterChanged filter -> { model with SelectedFilter = filter }, Command.none

let trackSelector (model: Model) (dispatch: Msg -> unit) =
  let data = model.Events |> List.distinctBy _.Track

  Shadcn.select [
    select.defaultValue "all"
    select.onValueChange (fun value ->
      match value with
      | "beginner" -> dispatch (FilterChanged(Tier Beginner))
      | "intermediate" -> dispatch (FilterChanged(Tier Intermediate))
      | "advanced" -> dispatch (FilterChanged(Tier Advanced))
      | _ -> dispatch (FilterChanged(Track value)))
    prop.children [
      Shadcn.selectTrigger [
        prop.className "w-[200px]"
        prop.children [
          Shadcn.selectValue [
            selectValue.placeholder (Html.text "Select a filter")
          ]
        ]
      ]
      Shadcn.selectContent [
        Shadcn.selectGroup [
          Shadcn.selectItem [
            prop.value "all"
            prop.text "All"
          ]
        ]
        Shadcn.selectGroup [
          Shadcn.selectLabel "Tier"
          Shadcn.selectItem [
            prop.value "beginner"
            prop.text "Beginner"
          ]
          Shadcn.selectItem [
            prop.value "intermediate"
            prop.text "Intermediate"
          ]
          Shadcn.selectItem [
            prop.value "advanced"
            prop.text "Advanced"
          ]
        ]
        Shadcn.selectGroup [
          Shadcn.selectLabel "Track"
          yield!
            data
            |> List.map (fun race ->
              Shadcn.selectItem [
                prop.value race.Track
                prop.text race.Track
              ])
        ]
      ]
    ]
  ]

let popover (event: RaceEvent) (open', setOpen) =
  Shadcn.popover [
    prop.custom ("open", open')
    prop.custom ("onOpenChange", setOpen)
    popover.open' open'
    prop.children [
      Shadcn.popoverTrigger [
        prop.custom ("data-state", if open' then "open" else "closed")
        prop.className "grow"
        prop.children [
          Html.div []
        ]
      ]
      Shadcn.popoverContent [
        prop.className "z-50"
        popoverContent.align.center
        popoverContent.side.top
        prop.children [
          Html.img [
            prop.className "w-full"
            prop.src event.ImageUrl
          ]
        ]
      ]
    ]
  ]

// [<ReactComponent>]
let RaceEventsComponent (model: Model) =
  let events =
    model.Events
    |> List.filter (fun event ->
      match model.SelectedFilter with
      | All -> true
      | Track track -> event.Track = track
      | Tier t -> event.Tier = t)
    |> List.collect (fun event ->
      event.Schedules
      |> List.map (fun schedule -> (schedule, event)))
    |> List.sortBy fst

  Html.div [
    prop.className "relative w-full overflow-x-auto"
    prop.children [
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
                // let open', setOpen = React.useState false
                // prop.onMouseEnter (fun _ -> setOpen true)

                prop.children [
                  Shadcn.tableCell $"""{dt.ToLocalTime().ToString("HH:mm")}"""
                  Shadcn.tableCell [
                    prop.className "flex"
                    prop.children [
                      Html.div [
                        prop.className "flex flex-col"
                        prop.children [
                          Html.div event.Title
                          Shadcn.badge [
                            prop.className "text-xs"
                            prop.text (event.Tier |> string)
                          ]
                        ]
                      ]
                    // popover event (open', setOpen)
                    ]
                  ]
                  Shadcn.tableCell event.Track
                  Shadcn.tableCell event.Duration
                ]
              ])
        ]
      ]
    ]
  ]

let topBar (_dispatch: Msg -> unit) (_model: Model) =
  Html.div [
    prop.className "m-3 flex flex-row gap-2"
    prop.children [
      Shadcn.button [
        prop.title "Refresh"
        prop.onClick (fun _ -> _dispatch Refresh)
        prop.disabled _model.IsLoading
        prop.children [
          Lucide.LoaderCircle [
            if _model.IsLoading then
              svg.className "animate-spin"
          ]
          Html.text "Refresh"
        ]
      ]
      // tierSelector _dispatch
      trackSelector _model _dispatch
    ]
  ]

let view (_model: Model) (_dispatch: Msg -> unit) =
  Html.div [
    topBar _dispatch _model
    RaceEventsComponent _model
  ]

// let appSideBar () =
//     Shadcn.sidebar [
//         Shadcn.sidebarContent [
//             Shadcn.sidebarGroup [
//                 Shadcn.sidebarGroupLabel "Application"
//                 Shadcn.sidebarGroupContent [
//                     Shadcn.sidebarMenu [
//                         Shadcn.sidebarMenuItem [
//                             prop.children [
//                                 Shadcn.sidebarMenuButton [
//                                     Html.a [
//                                         prop.href "https://www.racecontrol.gg"
//                                         prop.innerHtml "RaceControl"
//                                         prop.target.blank
//                                     ]
//                                 ]
//                             ]
//                         ]
//                     ]
//                 ]
//             ]
//         ]
//     ]
//
// let view (_model: Model) (_dispatch: Msg -> unit) =
//     if _model.IsLoading then
//         loadingComponent ()
//     else
//         Shadcn.sidebarProvider [
//             appSideBar ()
//             Shadcn.sidebarTrigger []
//             raceEventsComponent _model _dispatch
//         ]
//
let page (_shared: SharedModel) (_route: HomeRoute) =
  Page.from (fun _ -> init _shared) update view () LayoutMsg
