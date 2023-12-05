namespace Walrus

module Seq =

    let tryMax (items : seq<_>) =
        use e = items.GetEnumerator()
        if e.MoveNext () then
            let mutable maxItem = e.Current
            while e.MoveNext() do
                if e.Current > maxItem then
                    maxItem <- e.Current
            Some maxItem
        else None
