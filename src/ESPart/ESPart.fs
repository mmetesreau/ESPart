namespace ESPart

open ESPart.Types
open ESPart.Part

module ESPart =
    type ESContext = {
        request: Request option
        stream: Stream
        response: Stream option
    }

    type Evolve<'state, 'event> = 'state option -> 'event -> 'state option
    
    type CommandHandler<'command, 'state, 'event> = (AggregateId * 'command) -> 'state option -> 'event seq
    
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
                                |> Seq.map box                                

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
        | Some events ->
            events 
                |> Seq.map unbox<'event>
                |> eventHandler 
            return Some ctx     
        | _ -> return None
    }
    
    let createConsoleLogger () =
        let consoleHandler = 
            MailboxProcessor<ESContext>.Start(fun inbox ->
                let rec handler () = async {
                    let! ctx = inbox.Receive()

                    match ctx.request with 
                    | Some command ->
                        printfn "Log command: %A" command
                    | _ -> 
                        printfn "Log command: -"

                    match ctx.response with
                    | Some events ->
                        if events |> Seq.isEmpty  then
                            printfn "Log events: -"
                        else
                            events 
                                |> Seq.iter (printfn "Log : %A")
                    | _ ->      
                        printfn "Log events: -"

                    printfn "----------------"

                    return! handler()
                }

                handler ())

        fun (ctx: ESContext) -> async {
            consoleHandler.Post ctx

            return Some ctx
        }