namespace Mixter

open ESPart.Part
open ESPart.ESPart
open Mixter.Projections
open Mixter.Domain.Message

module App = 
    let createApp saveTimelineMessage removeTimelineMessage = 
        let consoleLogger = createConsoleLogger()

        let app =
            choose [
                aggregate message [
                    command quack            
                    command requack            
                    command delete            
                ] >=> project [
                    events (Timeline.handle saveTimelineMessage removeTimelineMessage)               
                ]
            ] >=> consoleLogger

        app        