namespace Walrus

open System

type Table =
    {
        Columns : Column[]
        ColumnMap : Map<string, Column>
        Rows : Row[]
    }

module Table =

    let create rows columns =
        {
            Columns = columns
            Rows = rows
            ColumnMap =
                columns
                    |> Seq.map (fun col -> col.Name, col)   // to-do: handle duplicate column names?
                    |> Map
        }

    let getValue colName getter table =
        getter table.ColumnMap[colName]

    (*
    let pivot rowGetValue colGetValue dataGetValue mapping table =
        table.Rows
            |> Seq.groupBy (rowGetValue table)
            |> Seq.map (fun (rowVal, rows) ->
                let triples =
                    rows
                        |> Seq.map (fun row ->
                            colGetValue table row,
                            dataGetValue table row)
                        |> Seq.groupBy fst
                        |> Seq.map (fun (colVal, pairs) ->
                            let pivotVal =
                                pairs
                                    |> Seq.map snd
                                    |> mapping
                            rowVal, colVal, pivotVal)
                        |> Seq.toArray
                let columns =
                    triples
                        |> Seq.map (fun (_, colVal, _) ->
                            colVal)
                        |> Seq.distinct
                        |> Seq.map (fun colVal ->
                            {
                                Name = string colVal
                                Type = 
                            }
                moo)
    *)

    let print table =

        for col in table.Columns do
            printf $" | {col.Name}"
        printfn " |"

        for col in table.Columns do
            printf $" | {col.Type}"
        printfn " |"

        for col in table.Columns do
            printf $" | {String('-', col.Name.Length)}"
        printfn " |"

        for row in table.Rows do
            for col in table.Columns do
                let strVal =
                    row
                        |> Row.getValue<obj> col
                        |> Option.map string
                        |> Option.defaultValue ""
                printf $" | {strVal}"
            printfn " |"
