namespace Walrus

open System
open System.IO

open CSVFile

module Csv =

    let private inferTypes nCols lines =

        let parserMap =
            Map [
                ColumnType.Float, (fun str ->
                    Double.TryParse(str : string) |> fst)
                ColumnType.Integer, (fun str ->
                    Int32.TryParse(str : string) |> fst)
            ]

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

    let private createRow columnTypes strings =
        Array.zip columnTypes strings
            |> Array.map (fun (colType, str) ->
                match colType with
                    | ColumnType.Float ->
                        if String.IsNullOrEmpty(str) then null
                        else Double.Parse(str) |> box
                    | ColumnType.Integer ->
                        if String.IsNullOrEmpty(str) then null
                        else Int32.Parse(str) |> box
                    | ColumnType.String -> box str)
            |> Row.ofValues

    let loadTable path =
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
                    createRow colTypes line
            |]
        Table.create rows columns
