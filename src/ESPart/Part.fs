namespace ESPart

module Part = 
    type Part<'a> = 'a -> Async<'a option>

    let bind (f: 'a -> Async<'b option>) (a: Async<'a option>) = async {
        let! p = a
        match p with
        | None ->
            return None
        | Some q ->
            let r = f q
            return! r
    }

    let compose (first: 'a -> Async<'b option>) (second: 'b -> Async<'c option>) =
        fun x -> bind second (first x)

    let (>=>) a b = 
        compose a b

    let rec choose (options: Part<'a> list) : Part<'a> =
        fun arg -> async {
            match options with
            | []        -> return None
            | p :: tail ->
                let! res = p arg
                match res with
                | Some x -> return Some x
                | None   -> return! choose tail arg
        }