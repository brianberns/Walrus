﻿namespace Walrus

open System
open System.IO

open CSVFile

module internal Option =

    /// Boxes the contained value.
    let box opt =
        opt
            |> Option.map box
            |> Option.defaultValue null

module internal Csv =

    type private ColumnType =
        | Boolean
        | Float
        | Integer
        | String

    let private parserMap =

        let boolParser = function
            | "0" -> Some false
            | "1" -> Some true
            | str ->
                let parsed, value = Boolean.TryParse(str)
                if parsed then Some value else None

        let floatParser str =
            let parsed, value = Double.TryParse(str : string)
            if parsed then Some value else None

        let integerParser str =
            let parsed, value = Int32.TryParse(str : string)
            if parsed then Some value else None

        Map [
            Boolean, boolParser >> Option.box
            Float, floatParser >> Option.box
            Integer, integerParser >> Option.box
            String, box
        ]

    let private colTypePriority =
        [|
            Boolean
            Integer
            Float
            String
        |]

    let private inferTypes nCols lines =

        let colTypeSets =
            Array.replicate nCols (set parserMap.Keys)

        (colTypeSets, lines)
            ||> Seq.fold (fun acc (line : string[]) ->
                [|
                    for iCol = 0 to nCols - 1 do
                        acc[iCol]
                            |> Set.filter (fun colType ->
                                parserMap[colType] line[iCol]
                                    |> isNull
                                    |> not)
                |])
            |> Seq.map (fun colTypes ->
                colTypePriority
                    |> Seq.find (fun ct -> colTypes.Contains(ct)))
            |> Seq.toArray

    let private createRow columnTypes strings =
        Array.zip columnTypes strings
            |> Array.map (fun (colType, str) ->
                parserMap[colType] str)

    let loadFile path =

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
        headers, rows
