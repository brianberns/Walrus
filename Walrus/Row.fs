namespace Walrus

type Row =
    {
        Values : obj[]
    }

module Row =

    let ofValues values =
        { Values = values }

    let getValue<'t> column row =
        let value = row.Values[column.Index]
        if isNull value then None
        else Some (value :?> 't)
