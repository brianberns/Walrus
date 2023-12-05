namespace Walrus

type Row =
    {
        Values : obj[]
    }

module Row =

    let ofValues boxedValues =
        {
            Values = Seq.toArray boxedValues
        }

    let getValue<'t> iCol row =
        let value = row.Values[iCol]
        if isNull value then None
        else Some (value :?> 't)
