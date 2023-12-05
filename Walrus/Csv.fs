namespace Walrus

open System
open System.IO

open CSVFile

module Csv =

    type private ColumnType =
        | Float
        | Integer
        | String

    let private inferTypes nCols lines =

        let parserMap =
            Map [
                Float, (fun str ->
                    Double.TryParse(str : string) |> fst)
                Integer, (fun str ->
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
                        if colTypes.Contains(Integer) then
                            assert(colTypes.Contains(Float))
                            Integer
                        else String))
            |> Seq.toArray

    let private createRow columnTypes strings =
        Array.zip columnTypes strings
            |> Array.map (fun (colType, str) ->
                match colType with
                    | Float ->
                        if String.IsNullOrEmpty(str) then null
                        else Double.Parse(str) |> box
                    | Integer ->
                        if String.IsNullOrEmpty(str) then null
                        else Int32.Parse(str) |> box
                    | String -> box str)
            |> Row.ofValues

    let loadTable path =

        let headers, lines =
            use reader = new StreamReader(path : string)
            use reader = new CSVReader(reader)
            let lines = reader.Lines() |> Seq.toArray
            reader.Headers, lines

        let colTypes = inferTypes headers.Length lines
        let rows =
            [|
                for line in lines do
                    createRow colTypes line
            |]
        Table.create headers rows
