namespace Walrus

open System
open System.IO

open CSVFile

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

    let private parserMap =
        Map [
            ColumnType.Float, (fun str ->
                Double.TryParse(str : string) |> fst)
            ColumnType.Integer, (fun str ->
                Int32.TryParse(str : string) |> fst)
        ]

    let private inferTypes nCols lines =

        let colTypeSets =
            Array.replicate nCols (set parserMap.Keys)
        (colTypeSets, lines)
            ||> Seq.fold (fun acc (line : string[]) ->
                [|
                    for iCol = 0 to nCols - 1 do
                        acc[iCol]
                            |> Set.filter (fun colType ->
                                let str = line[iCol]
                                String.IsNullOrEmpty(str)
                                    || parserMap[colType] str)
                |])
            |> Seq.map (fun colTypes ->
                Seq.tryExactlyOne colTypes
                    |> Option.defaultWith (fun () ->
                        if colTypes.Contains(ColumnType.Integer) then
                            assert(colTypes.Contains(ColumnType.Float))
                            ColumnType.Integer
                        else ColumnType.String))
            |> Seq.toArray

    let readCsv path =
        use reader = new StreamReader(path : string)
        use reader = new CSVReader(reader)
        let lines = reader.Lines() |> Seq.toArray
        let headers = reader.Headers
        let colTypes = inferTypes headers.Length lines
        let columns =
            ((0, 0, 0), Array.zip headers colTypes)
                ||> Array.mapFold (fun (iFloat, iInt, iStr) (colName, colType) ->
                    let acc, idx =
                        match colType with
                            | ColumnType.Float ->
                                (iFloat + 1, iInt, iStr), iFloat
                            | ColumnType.Integer ->
                                (iFloat, iInt + 1, iStr), iInt
                            | ColumnType.String ->
                                (iFloat, iInt, iStr + 1), iStr
                    let column =
                        {
                            Name = colName
                            Type = colType
                            Index = idx
                        }
                    column, acc)
                |> fst
        let rows =
            [|
                for line in lines do
                    Row.ofStrings colTypes line
            |]
        create rows columns

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
