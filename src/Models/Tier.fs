module Tier

open Thoth.Json

type Tier =
    | All
    | Beginner
    | Intermediate
    | Advanced

    static member FromString(s: string) =
        match s.ToLower() with
        | "all" -> All
        | "beginner" -> Beginner
        | "intermediate" -> Intermediate
        | "advanced" -> Advanced
        | x -> failwith $"Invalid tier {x}"

module Decoders =
    let tierDecoder: Decoder<Tier> =
        Decode.string
        |> Decode.andThen (fun str ->
            match str.ToLower() with
            | "all" -> Decode.succeed Tier.All
            | "beginner" -> Decode.succeed Tier.Beginner
            | "intermediate" -> Decode.succeed Tier.Intermediate
            | "advanced" -> Decode.succeed Tier.Advanced
            | _ -> Decode.fail $"Impossible de convertir '{str}' en Tier")

    let tierEncoder (tier: Tier) =
        match tier with
        | Tier.All -> Encode.string "all"
        | Tier.Beginner -> Encode.string "beginner"
        | Tier.Intermediate -> Encode.string "intermediate"
        | Tier.Advanced -> Encode.string "advanced"
