namespace Walrus

open System
open System.IO

open CSVFile

module Seq =

    let tryMax (items : seq<_>) =
        use e = items.GetEnumerator()
        if e.MoveNext () then
            let mutable maxItem = e.Current
            while e.MoveNext() do
                if e.Current > maxItem then
                    maxItem <- e.Current
            Some maxItem
        else None

[<RequireQualifiedAccess>]
type ColumnType =
    | Float
    | Integer
    | String

type Column =
    {
        Name : string
        Type : ColumnType
        Index : int
    }

type Row =
    {
        FloatValues : Option<float>[]
        IntegerValues : Option<int64>[]
        StringValues : Option<string>[]
    }

module Row =

    let ofValues columns values =
        let pairs = Array.zip columns values
        let floatVals, intVals, strVals =
            (pairs, ([], [], []))
                ||> Seq.foldBack (fun (col, str) (floatVals, intVals, strVals) ->
                    match col with
                        | ColumnType.Float ->
                            let valOpt =
                                if String.IsNullOrEmpty(str) then None
                                else Double.Parse(str) |> Some
                            valOpt :: floatVals, intVals, strVals
                        | ColumnType.Integer ->
                            let valOpt =
                                if String.IsNullOrEmpty(str) then None
                                else Int64.Parse(str) |> Some
                            floatVals, valOpt :: intVals, strVals
                        | ColumnType.String ->
                            let valOpt =
                                if isNull str then None
                                else Some str
                            floatVals, intVals, valOpt :: strVals)
        {
            FloatValues = Seq.toArray floatVals
            IntegerValues = Seq.toArray intVals
            StringValues = Seq.toArray strVals
        }

    let getValue column row =
        match column.Type with
            | ColumnType.Float ->
                row.FloatValues[column.Index]
                    |> Option.map box
            | ColumnType.Integer ->
                row.IntegerValues[column.Index]
                    |> Option.map box
            | ColumnType.String ->
                row.StringValues[column.Index]
                    |> Option.map box

type Table =
    {
        Columns : Column[]
        Rows : Row[]
    }

module Table =

    let private parserMap =
        Map [
            ColumnType.Float, (fun str ->
                Double.TryParse(str : string) |> fst)
            ColumnType.Integer, (fun str ->
                Int64.TryParse(str : string) |> fst)
        ]

    let private inferTypes nCols lines =

        let dbTypeSets =
            Array.replicate nCols (set parserMap.Keys)
        (dbTypeSets, lines)
            ||> Seq.fold (fun acc (line : string[]) ->
                [|
                    for iCol = 0 to nCols - 1 do
                        acc[iCol]
                            |> Set.filter (fun dbType ->
                                let str = line[iCol]
                                String.IsNullOrEmpty(str)
                                    || parserMap[dbType] str)
                |])
            |> Seq.map (fun dbTypes ->
                Seq.tryExactlyOne dbTypes
                    |> Option.defaultWith (fun () ->
                        if dbTypes.Contains(ColumnType.Integer) then
                            assert(dbTypes.Contains(ColumnType.Float))
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
            Seq.zip headers colTypes
                |> Seq.indexed
                |> Seq.groupBy (fun (_, (_, colType)) -> colType)
                |> Seq.collect (fun (_, group) ->
                    group
                        |> Seq.mapi (fun idx (iCol, (colName, colType)) ->
                            let col =
                                {
                                    Name = colName
                                    Type = colType
                                    Index = idx
                                }
                            col, iCol))
                |> Seq.sortBy snd
                |> Seq.map fst
                |> Seq.toArray
        let rows =
            [|
                for line in lines do
                    Row.ofValues colTypes line
            |]
        {
            Columns = columns
            Rows = rows
        }

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
                        |> Row.getValue col
                        |> Option.map string
                        |> Option.defaultValue ""
                printf $" | {strVal}"
            printfn " |"
