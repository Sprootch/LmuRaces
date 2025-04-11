module LmuRaces.Shared

open System
open ElmishLand

type SharedModel = {
    ApiUrl : string
}

type SharedMsg = | NoOp

let init () =
    { ApiUrl = "https://localhost:5001/api" }, Command.none

let update (msg: SharedMsg) (model: SharedModel) =
    match msg with
    | NoOp -> model, Command.none

// https://elmish.github.io/elmish/docs/subscription.html
let subscriptions _model : (string list * ((SharedMsg -> unit) -> IDisposable)) list = []
