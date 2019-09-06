namespace ESPart.Types

open System

type Stream = obj seq
type Event = obj
type AggregateId = Guid
type Command = obj
type Request = AggregateId * Command
