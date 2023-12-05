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

module private Sqlite =

    let conn = new SQLiteConnection("Data Source=:memory:")

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

    let readCsv path =
        use reader = new StreamReader(path : string)
        use reader = new CSVReader(reader)
        let nCols = reader.Headers.Length
        inferTypes nCols (reader.Lines())
