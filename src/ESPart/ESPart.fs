namespace ESPart

open ESPart.Types
open ESPart.Part

module ESPart =
    type ESContext = {
        request: Request option
        stream: Stream
        response: Result<Event seq, string> option
    }

    type Evolve<'state, 'event> = 'state option -> 'event -> 'state option
    
    type CommandHandler<'command, 'state, 'event> = (AggregateId * 'command) -> 'state option -> Result<'event seq, string>
    
    type EventHandler<'event> = 'event seq -> unit

    let rec aggregate (evolve: Evolve<'state,'event>) (commandHandlers: (Evolve<'state, 'event> -> Part<ESContext>) list) (ctx: ESContext) = async {
        match commandHandlers with
        | []        -> return None
        | p :: tail ->
            match! p evolve ctx with
            | Some x -> return Some x
            | None   -> return! aggregate evolve tail ctx
    }

    let command (commandHandler: CommandHandler<'command, 'state, 'event>) (evolve: Evolve<'state, 'event>) (ctx: ESContext) = async {        
        match ctx.request with
        | Some (aggregateId, command) ->
            match command with
            | :? 'command as command ->
                let events = ctx.stream 
                                |> Seq.map unbox<'event>
                                |> Seq.fold evolve None
                                |> commandHandler (aggregateId, command)
                                |> Result.bind (fun events -> events |> Seq.map box |> Ok)

                return Some { ctx with response = Some events }     
            | _ -> return None                                         
        | _ -> return None
    }

    let rec project (eventHandlers: Part<ESContext> list) (ctx: ESContext) = async {
        for eventHandler in eventHandlers do
            let! r = eventHandler ctx
            ()

        return Some ctx        
    }

    let events (eventHandler: EventHandler<'event>) (ctx: ESContext) = async {
        match ctx.response with
        | Some (Ok events) ->
            events 
                |> Seq.map unbox<'event>
                |> eventHandler 
            return Some ctx     
        | _ -> return None
    }