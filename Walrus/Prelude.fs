namespace Walrus

module Seq =

    /// Answers the maximum item in the given sequence, or None if
    /// the sequence is empty.
    // https://stackoverflow.com/questions/62082930/best-way-to-do-trymax-and-trymin-in-f
    let tryMax (items : seq<_>) =
        use e = items.GetEnumerator()
        if e.MoveNext() then
            let mutable maxItem = e.Current
            while e.MoveNext() do
                if e.Current > maxItem then
                    maxItem <- e.Current
            Some maxItem
        else None

    /// Splits a sequence of pairs into two sequences.
    // https://stackoverflow.com/questions/37034919/how-to-do-seq-unzip
    let unzip items =
        let cached = Seq.cache items
        Seq.map fst cached, Seq.map snd cached
