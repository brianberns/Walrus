namespace Walrus

open System

/// Describes a column of a specific type.
type Column<'t> =
    {
        Name : string
    }

[<AutoOpen>]
module Column =

    /// Describes a column of a specific type.
    let col<'t> name : Column<'t> =
        {
            Name = name
        }

type Table =
    private {

        /// Names of this table's columns, in order.
        ColumnNames : string[]

        /// Column indexes.
        ColumnMap : Map<string, int (*iCol*)>

        /// Rows in this table.
        InternalRows : InternalRow[]
    }

module Option =

    /// Boxes the contained value.
    let box opt =
        opt
            |> Option.map box
            |> Option.defaultValue null

module Table =

    /// Creates a table.
    let private create columnNames rows =
        {
            ColumnNames = Seq.toArray columnNames
            ColumnMap =
                columnNames
                    |> Seq.mapi (fun iCol name ->
                        name, iCol)
                    |> Map   // to-do: handle duplicate column names?
            InternalRows = Seq.toArray rows
        }

    /// Creates a table from the given row values.
    let ofRows columnNames rowValues =
        let rows =
            rowValues |> Seq.map InternalRow.create
        create columnNames rows

    /// Creates a new table with the rows ordered by the given
    /// column.
    let orderBy<'t when 't : comparison> columnName table =
        let rows =
            let iCol = table.ColumnMap[columnName]
            table.InternalRows
                |> Array.sortBy (InternalRow.getValue<'t> iCol)
        { table with InternalRows = rows }

    let getColumn<'t> columnName table =
        let iCol = table.ColumnMap[columnName]
        table.InternalRows
            |> Seq.map (InternalRow.getValue<'t> iCol)

    let tryGetColumn<'t> columnName table =
        let iCol = table.ColumnMap[columnName]
        table.InternalRows
            |> Seq.map (InternalRow.tryGetValue<'t> iCol)

    /// Creates a pivot table.
    let pivot
        (rowCol : Column<'row>)
        (colCol : Column<'col>)
        (dataCol : Column<'data>)
        (aggregate : seq<Option<'data>> -> 'agg)
        (getColName : Option<'col> -> string)
        table =

        let iRowCol = table.ColumnMap[rowCol.Name]
        let iColCol = table.ColumnMap[colCol.Name]
        let iDataCol = table.ColumnMap[dataCol.Name]

            // find distinct row values
        let rowMapPairs =
            table.InternalRows
                |> Seq.groupBy (InternalRow.tryGetValue<'row> iRowCol)
                |> Seq.map (fun (rowVal, rows) ->
                    let colAggMap =
                        rows
                            |> Seq.map (fun row ->
                                InternalRow.tryGetValue<'col> iColCol row,
                                InternalRow.tryGetValue<'data> iDataCol row)
                            |> Seq.groupBy fst
                            |> Seq.map (fun (colVal, pairs) ->
                                let aggVal : 'agg =
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
                rowCol.Name
                for colVal in colVals do
                    getColName colVal
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

    let mapRows mappings table =
        let colNames : string[] =
            Seq.map fst mappings
                |> Seq.toArray
        let rows =
            table.InternalRows
                |> Array.map (fun internalRow ->
                    let values =
                        mappings
                            |> Seq.map (fun (_, mapping) ->
                                let row =
                                    Row.create internalRow table.ColumnMap
                                mapping row |> box)
                    InternalRow.create values)
        create colNames rows

    let print table =

        let widths =
            [|
                for name in table.ColumnNames do
                    let strs =
                        seq {
                            name
                            for value in getColumn<obj> name table do
                                string value
                        }
                    strs
                        |> Seq.map String.length
                        |> Seq.max
            |]

        for name, width in Array.zip table.ColumnNames widths do
            printf " | %*s" width name
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

type Table with

    member table.Pivot(rowCol, colCol, dataCol, aggregate, ?getColName) =
        let getColName = defaultArg getColName string
        Table.pivot rowCol colCol dataCol aggregate getColName table
