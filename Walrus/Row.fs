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

    /// Answers the value of the given row in the given column
    /// as the given type. No conversion is attempted, and an
    /// exception is thrown if the value is not of the given type.
    let getValue<'t> iCol row =
        unbox<'t> row.Values[iCol]

    /// Answers the value of the given row in the given column
    /// as the given type. No conversion is attempted, and an
    /// None is returned if the value is not of the given type.
    let tryGetValue<'t> iCol row =
        tryUnboxStrict<'t> row.Values[iCol]

/// A row in a table.
type Row =
    private {

        /// Column indexes from table.
        ColumnMap : Map<string, int (*iCol*)>

        /// Internal row values.
        InternalRow : InternalRow
    }

module Row =

    /// Creates a row.
    let internal create columnMap internalRow =
        {
            ColumnMap = columnMap
            InternalRow = internalRow
        }

    /// Unboxed value of the given column in the given row.
    let getValue<'t> columnName row =
        let iCol = row.ColumnMap[columnName]
        InternalRow.getValue<'t> iCol row.InternalRow

    /// Unboxed value of the given column in the given row.
    let tryGetValue<'t> columnName row =
        let iCol = row.ColumnMap[columnName]
        InternalRow.tryGetValue<'t> iCol row.InternalRow

type Row with

    /// Unboxed value of the given column in the given row.
    member row.GetValue<'t>(columnName) = Row.getValue<'t> columnName row

    /// Unboxed value of the given column in the given row.
    member row.TryGetValue<'t>(columnName) = Row.tryGetValue<'t> columnName row
