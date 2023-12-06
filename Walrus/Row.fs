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
            Values = Seq.toArray values
        }

    /// Unboxed value of the given column in the given row.
    /// Throws an exception if the value cannot be unboxed.
    let getValue<'t> iCol row =
        unbox<'t> row.Values[iCol]

    /// Unboxed value of the given column in the given row.
    /// Answers None if the value cannot be unboxed.
    let tryGetValue<'t> iCol row =
        tryUnbox<'t> row.Values[iCol]
