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

    let createParameter dbType (cmd : SQLiteCommand) =
        let param = new SQLiteParameter(dbType : DbType)
        cmd.Parameters.Add(param) |> ignore
        param

type Column =
    {
        Name : string
        Type : DbType
    }

type Table =
    {
        Sql : string
        Columns : Column[]
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

        let tableName = $"table{nTables}"

        let headers, lines =
            use reader = new StreamReader(path : string)
            use reader = new CSVReader(reader)
            let lines =
                reader.Lines() |> Seq.toArray
            reader.Headers, lines

        let columns =
            let colTypes = inferTypes headers.Length lines
            use cmd =
                let sql =
                    let colDefs =
                        Seq.zip headers colTypes
                            |> Seq.map (fun (colName, colType) ->
                                $"[{colName}] {dataTypeMap[colType]}")
                            |> String.concat ", "
                    $"create table [{tableName}] ({colDefs})"
                SQLite.createCommand sql
            cmd.ExecuteNonQuery() |> ignore
            lock dataTypeMap
                (fun () ->
                    nTables <- nTables + 1)
            Array.zip headers colTypes
                |> Array.map (fun (colName, colType) ->
                    { Name = colName; Type = colType })

        use trans = SQLite.connection.BeginTransaction()
        use cmd =
            let colNamesStr =
                columns
                    |> Seq.map (fun col -> $"[{col.Name}]")
                    |> String.concat ", "
            let paramsStr =
                Seq.replicate columns.Length "?"
                    |> String.concat ", "
            SQLite.createCommand $"insert into [{tableName}] ({colNamesStr}) values ({paramsStr})"
        let parms =
            columns
                |> Array.mapi (fun iCol col ->
                    SQLite.createParameter col.Type cmd)
        for line in lines do
            Array.zip parms line
                |> Array.iter (fun (param, value) ->
                    param.Value <-
                        if String.IsNullOrEmpty value then
                            box DBNull.Value
                        else value)
            let nRows = cmd.ExecuteNonQuery()
            assert(nRows = 1)
        trans.Commit()

        {
            Sql = $"select * from {tableName}"
            Columns = columns
        }

    let print table =

        for col in table.Columns do
            printf $" | {col.Name}"
        printfn " |"

        for col in table.Columns do
            printf $" | {String('-', col.Name.Length)}"
        printfn " |"

        use cmd = SQLite.createCommand table.Sql
        use rdr = cmd.ExecuteReader()
        while rdr.Read() do
            for iCol = 0 to rdr.FieldCount - 1 do
                printf $" | {rdr.GetValue(iCol)}"
            printfn " |"
