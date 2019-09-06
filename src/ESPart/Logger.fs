namespace ESPart

open ESPart.ESPart 

module Logger =
    let createLogger (log: string -> unit) = 
        let consoleHandler = 
            MailboxProcessor<ESContext>.Start(fun inbox ->
                let rec handler () = async {
                    let! ctx = inbox.Receive()

                    match ctx.request with 
                    | Some command ->
                        sprintf "Log command: %A" command
                            |> log
                    | _ -> 
                        log "Log command: -"

                    match ctx.response with
                    | Some (Ok events) ->
                        events 
                            |> Seq.map (sprintf "Log event: %A")
                            |> Seq.iter log
                    | Some (Error error) ->  
                        sprintf "Log error: %s" error
                            |> log
                    | _ -> ()                        

                    log "----------------"

                    return! handler()
                }

                handler ())

        fun (ctx: ESContext) -> async {
            consoleHandler.Post ctx

            return Some ctx
        }