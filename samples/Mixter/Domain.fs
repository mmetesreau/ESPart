namespace Mixter.Domain

open ESPart.Types

[<AutoOpen>]
module Identity = 
    type UserId = { Email: string }
    type MessageId = AggregateId

module Message = 
    [<AutoOpen>]
    module Events = 
        type Event = 
            | MessageQuacked of MessageQuacked 
            | MessageRequacked of MessageRequacked 
            | MessageDeleted of MessageDeleted
        and MessageQuacked = { 
                MessageId: MessageId
                AuthorId: UserId
                Content: string
            } 
        and MessageRequacked = { 
                MessageId: MessageId
                RequackerId: UserId
            } 
        and MessageDeleted = { 
                MessageId: MessageId
                DeleterId: UserId
            }      
            
    [<AutoOpen>]
    module Commands =
        type Quack = { 
            AuthorId: UserId
            Content: string
        }

        type Requack = { 
            RequackerId : UserId 
        }    

        type Delete = {
            DeleterId: UserId
        }        

    type State = {
        MessageId: MessageId
        AuthorId: UserId; 
        Requackers: UserId list
        Deleted: bool
    }

    let message (state : State option) (event : Event) : State option =
        match state, event with
        | None, MessageQuacked messageQuacked -> 
            Some { MessageId = messageQuacked.MessageId; AuthorId = messageQuacked.AuthorId; Requackers = []; Deleted = false; }
        | Some message, MessageRequacked messageRequacked -> Some { message with Requackers = messageRequacked.RequackerId :: message.Requackers }
        | Some message, MessageDeleted _ -> Some { message with Deleted = true }
        | None, _ -> None
        | Some state, _ -> Some state  

    let quack (messageId : MessageId, command : Quack) (state : State option) =
        match state with
        | None -> 
            [ MessageQuacked { MessageId = messageId; AuthorId = command.AuthorId; Content = command.Content } ]
                |> Seq.ofList
                |> Ok
        | _ -> Error "Message already exist"

    let requack (messageId : MessageId, command : Requack) (state : State option) =
        match state with
        | Some message when message.AuthorId = command.RequackerId -> 
            Error "The author can't requack this own message"
        | Some message when message.Requackers |> List.exists ((=) command.RequackerId) -> 
            Error "The message has already been requacked by the user"
        | Some _  -> 
            [ MessageRequacked { MessageId = messageId; RequackerId = command.RequackerId } ]
                |> Seq.ofList
                |> Ok
        | None -> 
            Error "The message does not exist"

    let delete (messageId : MessageId, command : Delete) (state : State option) =
        match state with
        | Some message when message.Deleted ->
            Error "The message has already been deleted"
        | Some message when message.AuthorId = command.DeleterId ->
            Error "The message can already been deleted by its author"
        | Some _ -> 
            [ MessageDeleted { MessageId = messageId; DeleterId = command.DeleterId } ]
                |> Seq.ofList
                |> Ok
        | None -> 
            Error "The message does not exist"