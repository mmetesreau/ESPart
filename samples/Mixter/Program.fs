open ESPart.ESApp

open Mixter.Infrastructure
open Mixter.Domain.Message
open Mixter.App

open System

[<EntryPoint>]
let main _ =
    let timeLineStore = InMemoryTimelineMessageStore()

    let app = createApp timeLineStore.Save timeLineStore.Delete
    let commandDispacher = startApp inMemoryEventStore app
    
    let message1 = Guid.NewGuid()
    let message2 = Guid.NewGuid()
    
    { AuthorId = { Email = "toto@provider.com" } ; Content = "Hello"; }
        |> request message1
        |> commandDispacher.Post

    { RequackerId = { Email = "toto@provider.com" } }
        |> request message1
        |> commandDispacher.Post

    { RequackerId = { Email = "toto.tata@provider.com" } }
        |> request message1
        |> commandDispacher.Post

    { AuthorId = { Email = "toto.tata@provider.com" } ; Content = "Hello 2"; }
        |> request message2
        |> commandDispacher.Post

    { DeleterId = { Email = "toto.tata@provider.com" } }
        |> request message1
        |> commandDispacher.Post

    { DeleterId = { Email = "toto@provider.com" } }
        |> request message1
        |> commandDispacher.Post  

    { DeleterId = { Email = "toto@provider.com" } }
        |> request message1
        |> commandDispacher.Post      

    //printfn "Timeline toto@provider.com %A" (timeLineStore.GetMessagesOfUser { Email = "toto@provider.com" }) 
    //printfn "Timeline toto.tata@provider.com %A" (timeLineStore.GetMessagesOfUser { Email = "toto.tata@provider.com" }) 

    Console.ReadLine() |> ignore    
    0 