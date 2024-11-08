﻿namespace Walrus

open System

/// A table of data in rows and columns.
type Table =
    private {

        /// Names of this table's columns, in order.
        ColumnNames_ : string[]

        /// Column indexes.
        ColumnMap : Map<string, int (*iCol*)>

        /// Rows in this table.
        InternalRows : InternalRow[]
    }

    /// Number of columns in this table.
    member table.ColumnCount =
        table.ColumnNames_.Length

        /// Names of this table's columns, in order.
    member table.ColumnNames =
        table.ColumnNames_

    /// Number of rows in this table.
    member table.RowCount =
        table.InternalRows.Length

    /// Rows in this table.
    member table.Rows =
        table.InternalRows
            |> Seq.map (Row.create table.ColumnMap)

/// Ways of joining two tables.
[<RequireQualifiedAccess>]
type JoinType =
    | Inner
    | Left
    | Right
    | Outer

module Table =

    /// Creates a table from the given row values.
    let internal create columnNames rows =

        let columnNames = Seq.toArray columnNames
        for (row : InternalRow) in rows do
            if row.Values.Length <> columnNames.Length then
                $"Row length ({row.Values.Length}) does not match number of columns ({columnNames.Length})"
                    |> ArgumentException
                    |> raise

        {
            ColumnNames_ = Seq.toArray columnNames
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
    let loadCsvFile path =
        let columnNames, rowValues = Csv.loadFile path
        ofRows columnNames rowValues

    /// Creates a table from the given CSV string.
    let loadCsvString string =
        let columnNames, rowValues = Csv.loadString string
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
    let print (table : Table) =

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
            for iCol = 0 to table.ColumnCount - 1 do
                let width = widths[iCol]
                let strVal =
                    row
                        |> InternalRow.getValue<obj> iCol
                        |> string
                printf " | %*s" width strVal
            printfn " |"

    /// Answers the indexes of the given columns within the given
    /// table.
    let private getColumnIndexes columnNames table =
        columnNames
            |> Seq.map (fun colName ->
                table.ColumnMap[colName])

    /// Creates a table with rows filtered by the given predicate.
    let rowsWhere predicate table =
        {
            table with
                InternalRows =
                    table.Rows
                        |> Seq.where predicate
                        |> Seq.map (fun row -> row.InternalRow)
                        |> Seq.toArray
        }

    /// Creates a table with the rows ordered by the given columns.
    let sortRowsBy columnNames table =
        let rows =
            let iCols =
                getColumnIndexes columnNames table
                    |> Seq.toList
            table.InternalRows
                |> Array.sortBy (fun row ->
                    iCols
                        |> List.map (fun iCol ->
                            InternalRow.getValue iCol row))
        { table with InternalRows = rows }

    /// Creates a table with distinct rows.
    let distinct table =
        {
            table with
                InternalRows =
                    table.InternalRows
                        |> Array.distinct
        }

    /// Creates rows containing values from the given columns.
    let internal sliceRows colIndexes table =
        seq {
            for row in table.InternalRows do
                seq {
                    for iCol in colIndexes do
                        row.Values[iCol]
                } |> InternalRow.create
        }

    /// Creates a table containing only the given columns.
    let slice columnNames table =
        let rows =
            let iCols = getColumnIndexes columnNames table
            sliceRows iCols table
        create columnNames rows

    /// Maps the values of the given column.
    let mapColumn columnName mapping table =
        let rows =
            let iMapCol = table.ColumnMap[columnName]
            table.InternalRows
                |> Array.map (fun row ->
                    seq {
                        for iCol = 0 to table.ColumnCount - 1 do
                            if iCol = iMapCol then
                                row
                                    |> InternalRow.getValue<'t> iCol
                                    |> mapping
                                    |> box
                            else
                                InternalRow.getValue<obj> iCol row
                    } |> InternalRow.create)
        { table with InternalRows = rows }

    /// Maps and folds the values of the given column.
    let mapFoldColumn columnName folder state table =
        let rows, state' =
            let iMapCol = table.ColumnMap[columnName]
            table.InternalRows
                |> Array.mapFold (fun acc row ->
                    let value, acc' =
                        row
                            |> InternalRow.getValue<'t> iMapCol
                            |> folder acc
                    let row =
                        seq {
                            for iCol = 0 to table.ColumnCount - 1 do
                                if iCol = iMapCol then
                                    box value
                                else
                                    InternalRow.getValue<obj> iCol row
                        } |> InternalRow.create
                    row, acc') state
        { table with InternalRows = rows }, state'

    /// Creates a table with the given replacement column names.
    let renameColumns columnNames table =
        create columnNames table.InternalRows

    /// Creates a table with all columns from the given tables.
    let unionColumns (tableA : Table) (tableB : Table) =
        let columnNames =
            Array.append tableA.ColumnNames tableB.ColumnNames
        let rows =
            (tableA.InternalRows, tableB.InternalRows)
                ||> Array.map2 (fun rowA rowB ->
                    Array.append rowA.Values rowB.Values
                        |> InternalRow.create)
        create columnNames rows

    /// Creates a table with all rows from the given tables.
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

    /// Creates a table by joining the two given tables on the two
    /// given columns.
    let join joinType
        (tableA : Table, columnNameA)
        (tableB : Table, columnNameB) =

        let columnNames =
            Seq.append tableA.ColumnNames tableB.ColumnNames

        let rows =

                // prepare to lookup rows in each table
            let createMap table iCol =
                table.InternalRows
                    |> Seq.groupBy (InternalRow.getValue iCol)
                    |> Seq.where (fst >> isNull >> not)   // don't join on missing value
                    |> Map

                // creates a row
            let createRow rowAValues rowBValues =
                Seq.append rowAValues rowBValues
                    |> InternalRow.create

                // join column indexes
            let iColA = tableA.ColumnMap[columnNameA]
            let iColB = tableB.ColumnMap[columnNameB]

            seq {
                    // create rows driven by left table
                let rowMapB = createMap tableB iColB
                for rowA in tableA.InternalRows do
                    let rowBValuesSeq =
                        let value = InternalRow.getValue iColA rowA
                        match Map.tryFind value rowMapB with
                            | Some rowsB ->
                                rowsB |> Seq.map (fun row -> row.Values)
                            | None ->
                                match joinType with
                                    | JoinType.Left
                                    | JoinType.Outer ->
                                        Array.replicate tableB.ColumnCount null
                                            |> Seq.singleton
                                    | JoinType.Inner
                                    | JoinType.Right -> Seq.empty
                    for rowBValues in rowBValuesSeq do
                        yield createRow rowA.Values rowBValues

                    // create rows driven by right table
                match joinType with
                    | JoinType.Right
                    | JoinType.Outer ->
                        let rowMapA = createMap tableA iColA
                        for rowB in tableB.InternalRows do
                            let value = InternalRow.getValue iColB rowB
                            if Map.containsKey value rowMapA |> not then   // to-do: use a set of values instead of a map?
                                let rowAValues =
                                    Array.replicate tableA.ColumnCount null
                                yield createRow rowAValues rowB.Values
                    | JoinType.Inner
                    | JoinType.Left -> ()
            }

        create columnNames rows

    /// Creates a table by left-joining the two given tables on the two
    /// given columns.
    let leftJoin = join JoinType.Left

    /// Creates a table by inner-joining the two given tables on the two
    /// given columns.
    let innerJoin = join JoinType.Inner

    /// Creates a table by right-joining the two given tables on the two
    /// given columns.
    let rightJoin = join JoinType.Right

    /// Creates a table by outer-joining the two given tables on the two
    /// given columns.
    let outerJoin = join JoinType.Outer

    /// Answers values of the given row at the given columns.
    let private getValues iCols row =
        iCols
            |> List.map (fun iCol ->
                InternalRow.getValue<obj> iCol row)

    /// Groups the given table by the given columns, answering a subtable
    /// for each group.
    let groupBy colNames table =
        let iGroupCols =
            getColumnIndexes colNames table |> Seq.toList
        table.InternalRows
            |> Seq.groupBy (getValues iGroupCols)
            |> Seq.map (fun (key, rows) ->
                let subtable =
                    { table with
                        InternalRows = Seq.toArray rows }
                key, subtable)

    /// Groups the given table by the given columns, answering a subtable
    /// for each group.
    let groupBy1<'t> colName table =
        groupBy colName table
            |> Seq.map (fun (values, subtable) ->
                let key =
                    match values with
                        | [a] -> tryUnboxStrict<'t> a
                        | _ -> failwith "Unexpected"
                key, subtable)

    /// Groups the given table by the given columns, answering a subtable
    /// for each group.
    let groupBy2<'t, 'u> colNameA colNameB table =
        groupBy [colNameA; colNameB] table
            |> Seq.map (fun (values, subtable) ->
                let key =
                    match values with
                        | [a; b] ->
                            tryUnboxStrict<'t> a,
                            tryUnboxStrict<'u> b
                        | _ -> failwith "Unexpected"
                key, subtable)

    /// Groups the given table by the given columns, answering a subtable
    /// for each group.
    let groupBy3<'t, 'u, 'v> colNameA colNameB colNameC table =
        groupBy [colNameA; colNameB; colNameC] table
            |> Seq.map (fun (values, subtable) ->
                let key =
                    match values with
                        | [a; b; c] ->
                            tryUnboxStrict<'t> a,
                            tryUnboxStrict<'u> b,
                            tryUnboxStrict<'v> c
                        | _ -> failwith "Unexpected"
                key, subtable)

    /// Groups the given table on the given "group" columns, aggregating
    /// values in the given "agg" columns.
    let aggregateBy<'t, 'u>
        groupColNames
        aggColNames
        (aggregate : seq<Option<'t>> -> 'u)
        table =

        let columnNames = Seq.append groupColNames aggColNames

        let rows =
            let iGroupCols =
                getColumnIndexes groupColNames table |> Seq.toList
            let iAggCols = getColumnIndexes aggColNames table
            table.InternalRows
                |> Seq.groupBy (getValues iGroupCols)
                |> Seq.map (fun (rowVals, rows) ->
                    let aggValues =
                        seq {
                            for iAggCol in iAggCols do
                                seq {
                                    for row in rows do
                                        InternalRow.tryGetValue<'t> iAggCol row
                                } |> aggregate |> box
                        }
                    Seq.append rowVals aggValues
                        |> InternalRow.create)

        create columnNames rows

    /// Creates a pivot table, grouping on "row" columns, aggregating
    /// "data" column values for each distinct "column" column value.
    /// E.g. On the Titanic, count # of passengers (data column) who
    /// survived/died (column column) in each passenger class (row
    /// column).
    let pivotWith<'t, 'u>
        rowColNames
        colColName
        dataColName
        (aggregate : seq<Option<'t>> -> 'u)
        table =

            // get column indexes
        let iRowCols =
            getColumnIndexes rowColNames table |> Seq.toList
        let iColCol = table.ColumnMap[colColName]
        let iDataCol = table.ColumnMap[dataColName]

            // find distinct row values
        let rowMapPairs =
            table.InternalRows
                |> Seq.groupBy (getValues iRowCols)
                |> Seq.map (fun (rowVals, rows) ->
                    let colAggMap =
                        rows
                            |> Seq.map (fun row ->
                                InternalRow.getValue iColCol row,
                                InternalRow.tryGetValue<'t> iDataCol row)
                            |> Seq.groupBy fst
                            |> Seq.map (fun (colVal, pairs) ->
                                let aggVal =
                                    pairs
                                        |> Seq.map snd
                                        |> aggregate
                                colVal, aggVal)
                            |> Map
                    rowVals, colAggMap)
                |> Seq.toArray

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
                yield! rowColNames
                for colVal in colVals do
                    string colVal
            |]
        let rows =
            let noValue = lazy aggregate Seq.empty
            rowMapPairs
                |> Array.map (fun (rowVals, colAggMap) ->
                    seq {
                        yield! rowVals
                        for colVal in colVals do
                            colAggMap
                                |> Map.tryFind colVal
                                |> Option.defaultWith (fun () ->
                                    noValue.Value)
                                |> box
                    } |> InternalRow.create)
        create colNames rows

    /// Creates a pivot table, grouping on "row" columns, counting the
    /// number of rows for each distinct "column" column value.
    /// E.g. On the Titanic, count # of passengers (data column) who
    /// survived/died (column column) in each passenger class (row
    /// column).
    let pivot<'t>
        rowColNames
        colColName
        table =
            pivotWith<'t, int>
                rowColNames
                colColName
                colColName
                Seq.length
                table

type Table with

    /// Gets a numeric column from the given table.
    static member (?)(table : Table, columnName) =
        Table.getColumn<obj> columnName table
            |> Column.map Convert.ToDouble

    /// Creates a table containing a slice of columns.
    member private table.GetSlice(colIdxs) =
        let colNames =
            colIdxs
                |> Seq.map (fun iCol ->
                    table.ColumnNames[iCol])
        table
            |> Table.sliceRows colIdxs
            |> Table.create colNames

    /// Creates a table containing a slice of columns.
    member table.GetSlice(startColIdxOpt, endColIdxOpt) =
        let iStartCol =
            Option.defaultValue 0 startColIdxOpt
        let iEndCol =
            Option.defaultValue (table.ColumnCount - 1) endColIdxOpt
        table.GetSlice([| iStartCol .. iEndCol |])

    /// Creates a table containing a slice of columns.
    member table.GetSlice(startColNameOpt, endColNameOpt) =
        let getColIdx iDefault colNameOpt =
            colNameOpt
                |> Option.map (fun colName ->
                    table.ColumnMap[colName])
                |> Option.defaultValue iDefault
        let iStartCol = getColIdx 0 startColNameOpt
        let iEndCol =
            getColIdx (table.ColumnCount - 1) endColNameOpt
        table.GetSlice([| iStartCol .. iEndCol |])
