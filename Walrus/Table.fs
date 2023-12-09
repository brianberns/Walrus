namespace Walrus

open System

/// A table of data in rows and columns.
type Table =
    private {

        /// Names of this table's columns, in order.
        ColumnNames : string[]

        /// Column indexes.
        ColumnMap : Map<string, int (*iCol*)>

        /// Rows in this table.
        InternalRows : InternalRow[]
    }

    /// Rows in this table.
    member table.Rows =
        table.InternalRows
            |> Seq.map (Row.create table.ColumnMap)

module Table =

    (*
     * Access
     *)

    /// Gets the column with the given name from the given table.
    let getColumn<'t> columnName table =
        let iCol = table.ColumnMap[columnName]
        table.InternalRows
            |> Seq.map (InternalRow.getValue<'t> iCol)
            |> Column.create

    /// Gets the column with the given name from the given table
    /// with possibly missing values.
    let tryGetColumn<'t> columnName table =
        let iCol = table.ColumnMap[columnName]
        table.InternalRows
            |> Seq.map (InternalRow.tryGetValue<'t> iCol)
            |> Column.create

    /// Prints the given table to the console.
    let print table =

        let widths =
            [|
                for colName in table.ColumnNames do
                    let strs =
                        seq {
                            yield colName
                            let col = getColumn colName table
                            for value in col.Values do
                                yield string value
                        }
                    strs
                        |> Seq.map String.length
                        |> Seq.max
            |]

        for colName, width in Array.zip table.ColumnNames widths do
            printf " | %*s" width colName
        printfn " |"

        for _, width in Array.zip table.ColumnNames widths do
            printf " | %*s" width (String('-', width))
        printfn " |"

        for row in table.InternalRows do
            for iCol = 0 to table.ColumnNames.Length - 1 do
                let width = widths[iCol]
                let strVal =
                    row
                        |> InternalRow.getValue<obj> iCol
                        |> string
                printf " | %*s" width strVal
            printfn " |"

    (*
     * Creation
     *)

    /// Creates a table from the given row values.
    let private create columnNames rows =

        let columnNames = Seq.toArray columnNames
        for (row : InternalRow) in rows do
            if row.Values.Length <> columnNames.Length then
                failwith $"Invalid row length: {row.Values.Length}"

        {
            ColumnNames = Seq.toArray columnNames
            ColumnMap =
                columnNames
                    |> Seq.mapi (fun iCol colName ->
                        colName, iCol)
                    |> Map   // to-do: handle duplicate column names?
            InternalRows = Seq.toArray rows
        }

    /// Creates a table from the given row values.
    let ofRows columnNames rowValues =
        rowValues
            |> Seq.map InternalRow.create
            |> create columnNames

    /// Creates a table from the given columns.
    let ofColumns columnPairs =
        let columnNames, columns = Seq.unzip columnPairs
        seq {
            let nRows =
                columns
                    |> Seq.map (fun col -> col.Values.Length)
                    |> Seq.distinct
                    |> Seq.exactlyOne
            for iRow = 0 to nRows - 1 do
                Seq.map (Column.getValue iRow) columns
        } |> ofRows columnNames

    /// Creates a table from the given column.
    let ofColumn columnName column =
        ofColumns [| columnName, column |]

    /// Creates a table from the given CSV file.
    let loadCsv path =
        let columnNames, rowValues = Csv.loadFile path
        ofRows columnNames rowValues

    /// Creates a table from the given records.
    let ofRecords (records : seq<'t>) =
        let properties = typeof<'t>.GetProperties()
        let columnNames =
            properties |> Seq.map (fun prop -> prop.Name)
        seq {
            for rcd in records do
                seq {
                    for prop in properties do
                        prop.GetValue(rcd)
                }
        } |> ofRows columnNames

    (*
     * Manipulation
     *)

    /// Creates a new table with the rows ordered by the given
    /// column.
    let sortRowsBy<'t when 't : comparison> columnName table =
        let rows =
            let iCol = table.ColumnMap[columnName]
            table.InternalRows
                |> Array.sortBy (InternalRow.getValue<'t> iCol)
        { table with InternalRows = rows }

    /// Creates a new table with the given replacement column names.
    let renameColumns columnNames table =
        create columnNames table.InternalRows

    /// Creates a new table with all columns from the given tables.
    let unionColumns tableA tableB =
        let columnNames =
            Array.append tableA.ColumnNames tableB.ColumnNames
        let rows =
            (tableA.InternalRows, tableB.InternalRows)
                ||> Array.map2 (fun rowA rowB ->
                    Array.append rowA.Values rowB.Values
                        |> InternalRow.create)
        create columnNames rows

    /// Creates a new table with all rows from the given tables.
    let unionRows tableA tableB =
        seq {
                // tableA's rows
            yield! tableA.InternalRows

                // tableB's rows
            for row in tableB.InternalRows do
                seq {
                    for columnName in tableA.ColumnNames do
                        let iCol = tableB.ColumnMap[columnName]
                        row.Values[iCol]
                } |> InternalRow.create
        } |> create tableA.ColumnNames

    /// Creates a pivot table, grouping on "rowCol", aggregating "dataCol"
    /// values for each distinct "colCol" value. E.g. On the Titanic,
    /// count # of passengers (dataCol) who survived/died (colCol) in each
    /// passenger class (rowCol).
    let pivot<'t, 'u>
        rowColName
        colColName
        dataColName
        (aggregate : seq<Option<'t>> -> 'u)
        table =

        let iRowCol = table.ColumnMap[rowColName]
        let iColCol = table.ColumnMap[colColName]
        let iDataCol = table.ColumnMap[dataColName]

            // find distinct row values
        let rowMapPairs =
            table.InternalRows
                |> Seq.groupBy (InternalRow.tryGetValue iRowCol)
                |> Seq.map (fun (rowVal, rows) ->
                    let colAggMap =
                        rows
                            |> Seq.map (fun row ->
                                InternalRow.tryGetValue iColCol row,
                                InternalRow.tryGetValue<'t> iDataCol row)
                            |> Seq.groupBy fst
                            |> Seq.map (fun (colVal, pairs) ->
                                let aggVal =
                                    pairs
                                        |> Seq.map snd
                                        |> aggregate
                                colVal, aggVal)
                            |> Map
                    rowVal, colAggMap)

            // find distinct column values
        let colVals =
            rowMapPairs
                |> Seq.collect (fun (_, colAggMap) ->
                    colAggMap.Keys)
                |> Seq.distinct
                |> Seq.sort
                |> Seq.toArray

            // create table
        let colNames =
            [|
                rowColName
                for colVal in colVals do
                    string colVal
            |]
        let rows =
            rowMapPairs
                |> Seq.map (fun (rowVal, colAggMap) ->
                    seq {
                        Option.box rowVal
                        for colVal in colVals do
                            colAggMap
                                |> Map.tryFind colVal
                                |> Option.box
                    } |> InternalRow.create)
                |> Seq.toArray
        create colNames rows

type Table with

    /// Gets a numeric column from the given table.
    static member (?) (table : Table, columnName) =
        Table.getColumn<obj> columnName table
            |> Column.map Convert.ToDouble
