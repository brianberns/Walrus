namespace Walrus

type Row =
    private {

        /// Boxed values for this row.
        Values : obj[]
    }

module Row =

    /// Creates a row from the given values.
    let create values =
        {
            Values =
                values
                    |> Seq.cast<obj>
                    |> Seq.toArray
        }

    /// Unboxed value of the given column in the given row.
    let getValue<'t> iCol row =
        unbox<'t> row.Values[iCol]

    /// Unboxed value of the given column in the given row.
    let tryGetValue<'t> iCol row =
        let value = row.Values[iCol]
        if isNull value then None
        else unbox<'t> value |> Some
