namespace Mixter

open ESPart.Types
open ESPart.Configuration

open Mixter.Projections.Timeline

open System.Collections.Generic

module Infrastructure = 
    type InMemoryTimelineMessageStore() =
        let store = new HashSet<TimelineMessage>()

        member __.Save timelineMessage =
            store.Add timelineMessage |> ignore

        member __.GetMessagesOfUser userId =
            store |> Seq.filter (fun p -> p.Owner = userId)

        member __.Delete messageId =
            store.RemoveWhere(fun p -> p.MessageId = messageId) |> ignore

    type InMemoryEventStore = {
        mutable streams : Map<AggregateId, Stream>
    } with interface IEventStore with 
            member self.Load (streamId : AggregateId) : Async<Stream> = async {
                return self.streams 
                        |> Map.tryFind streamId
                        |> Option.defaultValue Seq.empty
            }

            member self.Append (streamId : AggregateId) (appendToStream : Stream) : Async<unit> = async { 
                let stream = 
                    self.streams 
                        |> Map.tryFind streamId
                        |> Option.defaultValue Seq.empty
                        |> Seq.append appendToStream

                self.streams <- self.streams |> Map.add streamId stream
                ()
            }

    let inMemoryEventStore = { streams = Map.empty } 