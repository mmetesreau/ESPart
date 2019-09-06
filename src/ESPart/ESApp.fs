namespace ESPart

open ESPart.Types
open ESPart.Configuration
open ESPart.Part
open ESPart.ESPart

module ESApp = 
    let run (ctx: ESContext) (esPart: Part<ESContext>) = async {
            try
                return! esPart ctx
            with e ->
                printfn "%A" e
                return None
        }

    let startAggregate (aggregateId: AggregateId) (eventStore: IEventStore) (esPart: Part<ESContext>) = async {
        let! stream = eventStore.Load aggregateId
        
        return MailboxProcessor<Request>.Start(fun inbox ->
            let rec commandLoop ctx = async {
                let! aggregateId, command = inbox.Receive()

                match! run { ctx with request = Some (aggregateId, command) } esPart with
                | Some newCtx ->
                    match newCtx.response with
                    | Some events ->
                        do! eventStore.Append aggregateId events
                        
                        let newStream = events |> Seq.append newCtx.stream
                        
                        return! commandLoop { newCtx with stream = newStream; response = None }              
                    | None -> 
                        return! commandLoop { newCtx with response = None }              
                | None ->
                    return! commandLoop { ctx with response = None }              
            }

            commandLoop { stream = stream; request = None; response = None })  
    }

    let startApp (eventStore: IEventStore) (esPart: Part<ESContext>) =
        MailboxProcessor<Request>.Start(fun inbox ->
            let rec commandDispacher (aggregates: Map<AggregateId, MailboxProcessor<Request>>) = async {
                let! aggregateId, command = inbox.Receive()

                match Map.tryFind aggregateId aggregates with
                | Some aggregate ->

                    aggregate.Post (aggregateId, command)

                    return! commandDispacher aggregates
                | None -> 

                    let! aggregate = startAggregate aggregateId eventStore esPart

                    let aggregates = Map.add aggregateId aggregate aggregates

                    aggregate.Post (aggregateId, command)

                    return! commandDispacher aggregates
            }

            commandDispacher Map.empty<AggregateId, MailboxProcessor<Request>>)

    let inline request (aggregateId: AggregateId) (command: 'a) =
        aggregateId, box command