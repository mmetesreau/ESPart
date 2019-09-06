namespace Mixter

open Mixter.Domain.Identity
open Mixter.Domain.Message.Events

module Projections = 
    module Timeline = 
        type TimelineMessage = { 
            Owner: UserId
            Author: UserId 
            Content: string 
            MessageId: MessageId 
        }

        let handle (save: TimelineMessage -> unit) (remove: MessageId -> unit) (events: Event seq) =
            events
                |> Seq.iter (fun evt -> 
                                match evt with
                                | MessageQuacked e -> save { Owner = e.AuthorId; Author = e.AuthorId; Content = e.Content; MessageId = e.MessageId }
                                | MessageDeleted e -> remove e.MessageId
                                | _ -> ())