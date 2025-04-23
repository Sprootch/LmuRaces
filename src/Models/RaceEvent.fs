module RaceEvent

open System
open Tier

type RaceEvent = {
    Title: string
    Tier: Tier
    Track: string
    Duration: string
    DurationTs: TimeSpan
    ImageUrl: string
    Schedules: DateTimeOffset List
}
