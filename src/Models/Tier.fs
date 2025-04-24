module Tier

open Thoth.Json

type Tier =
    | Beginner
    | Intermediate
    | Advanced

    static member FromString(tier: string) =
        match tier.ToLower() with
        | "beginner" -> Some Beginner
        | "intermediate" -> Some Intermediate
        | "advanced" -> Some Advanced
        | _ -> None

module Decoders =
    let tierDecoder: Decoder<Tier> =
        Decode.string
        |> Decode.andThen (fun str ->
            match str.ToLower() with
            | "beginner" -> Decode.succeed Tier.Beginner
            | "intermediate" -> Decode.succeed Tier.Intermediate
            | "advanced" -> Decode.succeed Tier.Advanced
            | _ -> Decode.fail $"Impossible de convertir '{str}' en Tier")

    let tierEncoder (tier: Tier) =
        match tier with
        | Tier.Beginner -> Encode.string "beginner"
        | Tier.Intermediate -> Encode.string "intermediate"
        | Tier.Advanced -> Encode.string "advanced"
