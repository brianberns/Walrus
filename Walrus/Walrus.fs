namespace Walrus

open System
open System.IO
open System.Data
open System.Data.SQLite

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

module private SQLite =

    let connection =
        let conn = new SQLiteConnection("Data Source=:memory:")
        conn.Open()
        conn

    let createCommand commandText =
        new SQLiteCommand(commandText, connection)

type Table =
    {
        Name : string
    }

module Table =

    let private parserMap =
        Map [
            DbType.Double, (fun str ->
                Double.TryParse(str : string) |> fst)
            DbType.Int64, (fun str ->
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
                        if dbTypes.Contains(DbType.Int64) then
                            assert(dbTypes.Contains(DbType.Double))
                            DbType.Int64
                        else DbType.String))
            |> Seq.toArray

    let private dataTypeMap =
        Map [
            DbType.Double, "real"
            DbType.Int64, "integer"
            DbType.String, "text"
        ]

    let mutable private nTables = 0

    let readCsv path =

        use reader = new StreamReader(path : string)
        use reader = new CSVReader(reader)

        let tableName = $"table{nTables}"
        use cmd =
            let sql =
                let colDefs =
                    let colTypes =
                        inferTypes
                            reader.Headers.Length
                            (reader.Lines())
                    Seq.zip reader.Headers colTypes
                        |> Seq.map (fun (colName, colType) ->
                            $"{colName} {dataTypeMap[colType]}")
                        |> String.concat ", "
                $"create table {tableName} ({colDefs})"
            SQLite.createCommand sql
        cmd.ExecuteNonQuery() |> ignore
        lock dataTypeMap
            (fun () ->
                nTables <- nTables + 1)

        { Name = tableName }
