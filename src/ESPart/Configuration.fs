namespace ESPart.Configuration

open ESPart.Types

type IEventStore = interface
    abstract member Load : AggregateId -> Async<Stream>
    abstract member Append : AggregateId -> Stream -> Async<unit>
end