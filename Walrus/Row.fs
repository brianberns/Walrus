namespace Walrus

/// Internal row type.
type internal InternalRow =
    {
        /// Boxed values for this row.
        Values : obj[]
    }

module internal InternalRow =

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

/// External row type.
type Row =
    private {

        InternalRow : InternalRow

        /// Column indexes.
        ColumnMap : Map<string, int (*iCol*)>
    }

module Row =

    let internal create internalRow columnMap =
        {
            InternalRow = internalRow
            ColumnMap = columnMap
        }

    /// Unboxed value of the given column in the given row.
    let getValue<'t> columnName row =
        let iCol = row.ColumnMap[columnName]
        InternalRow.getValue<'t> iCol row.InternalRow

    /// Unboxed value of the given column in the given row.
    let tryGetValue<'t> columnName row =
        let iCol = row.ColumnMap[columnName]
        InternalRow.tryGetValue<'t> iCol row.InternalRow
