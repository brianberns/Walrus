namespace Walrus

open System

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
    {
        ColumnNames : string[]
        ColumnMap : Map<string, int>
        Rows : Row[]
    }

module Table =

    let create columnNames rows =
        {
            ColumnNames = columnNames
            ColumnMap =
                columnNames
                    |> Seq.mapi (fun iCol name ->
                        name, iCol)   // to-do: handle duplicate column names?
                    |> Map
            Rows = rows
        }

    let getValue colName getter table =
        getter table.ColumnMap[colName]

    let pivot
        (rowCol : Column<'row>)
        (colCol : Column<'col>)
        (dataCol : Column<'data>)
        (mapping : seq<Option<'data>> -> 'agg) table =
        let iRowCol = table.ColumnMap[rowCol.Name]
        let iColCol = table.ColumnMap[colCol.Name]
        let iDataCol = table.ColumnMap[dataCol.Name]
        let pairs =
            table.Rows
                |> Seq.groupBy (Row.getValue<'row> iRowCol)
                |> Seq.map (fun (rowVal, rows) ->
                    let colAggMap =
                        rows
                            |> Seq.map (fun row ->
                                Row.getValue<'col> iColCol row,
                                Row.getValue<'data> iDataCol row)
                            |> Seq.groupBy fst
                            |> Seq.map (fun (colVal, pairs) ->
                                let aggVal =
                                    pairs
                                        |> Seq.map snd
                                        |> mapping
                                colVal, aggVal)
                            |> Map
                    (*
                    let rows =
                        triples
                            |> Seq.groupBy (fun (rowVal, _, _) ->
                                rowVal)
                            |> Seq.map (fun (rowVal, group) ->)
                    *)
                    rowVal, colAggMap)
        let colVals =
            pairs
                |> Seq.collect (fun (_, colAggMap) ->
                    colAggMap.Keys)
                |> Seq.distinct
                |> Seq.sort
                |> Seq.toArray
        let columnNames =
            [|
                rowCol.Name
                for colVal in colVals do
                    string colVal
            |]
        let boxed opt =
            opt
                |> Option.map box
                |> Option.defaultValue null
        let rows =
            pairs
                |> Seq.map (fun (rowVal, colAggMap) ->
                    seq {
                        boxed rowVal
                        for colVal in colVals do
                            colAggMap
                                |> Map.tryFind colVal
                                |> boxed
                    } |> Row.ofValues)
                |> Seq.toArray
        create columnNames rows

    let print table =

        for name in table.ColumnNames do
            printf $" | {name}"
        printfn " |"

        for name in table.ColumnNames do
            printf $" | {String('-', name.Length)}"
        printfn " |"

        for row in table.Rows do
            for iCol = 0 to table.ColumnNames.Length - 1 do
                let strVal =
                    row
                        |> Row.getValue<obj> iCol
                        |> Option.map string
                        |> Option.defaultValue ""
                printf $" | {strVal}"
            printfn " |"
