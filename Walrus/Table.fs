namespace Walrus

open System

/// Typed column in a table.
type Column<'t> =
    {
        Name : string
    }

[<AutoOpen>]
module Column =

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
        Rows : Row[]
    }

module Option =

    /// Boxes the contained value.
    let box opt =
        opt
            |> Option.map box
            |> Option.defaultValue null

module Table =

    /// Creates a table.
    let create columnNames rows =
        {
            ColumnNames = columnNames
            ColumnMap =
                columnNames
                    |> Seq.mapi (fun iCol name ->
                        name, iCol)
                    |> Map   // to-do: handle duplicate column names?
            Rows = rows
        }

    let orderBy<'t when 't : comparison> columnName table =
        let iCol = table.ColumnMap[columnName]
        let rows =
            table.Rows
                |> Array.sortBy (Row.getValue<'t> iCol)
        { table with Rows = rows }

    let getValues<'t> columnName table =
        let iCol = table.ColumnMap[columnName]
        table.Rows
            |> Seq.map (Row.getValue<'t> iCol)

    let tryGetValues<'t> columnName table =
        let iCol = table.ColumnMap[columnName]
        table.Rows
            |> Seq.map (Row.tryGetValue<'t> iCol)

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
            table.Rows
                |> Seq.groupBy (Row.tryGetValue<'row> iRowCol)
                |> Seq.map (fun (rowVal, rows) ->
                    let colAggMap =
                        rows
                            |> Seq.map (fun row ->
                                Row.tryGetValue<'col> iColCol row,
                                Row.tryGetValue<'data> iDataCol row)
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
        let columnNames =
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
                    } |> Row.create)
                |> Seq.toArray
        create columnNames rows

    let print table =

        let widths =
            [|
                for name in table.ColumnNames do
                    let strs =
                        seq {
                            name
                            for value in getValues<obj> name table do
                                string value
                        }
                    strs
                        |> Seq.map String.length
                        |> Seq.max
            |]

        for name, width in Array.zip table.ColumnNames widths do
            printf " | %*s" width name
        printfn " |"

        for name, width in Array.zip table.ColumnNames widths do
            printf " | %*s" width (String('-', name.Length))
        printfn " |"

        for row in table.Rows do
            for iCol = 0 to table.ColumnNames.Length - 1 do
                let width = widths[iCol]
                let strVal =
                    row
                        |> Row.getValue<obj> iCol
                        |> string
                printf " | %*s" width strVal
            printfn " |"
