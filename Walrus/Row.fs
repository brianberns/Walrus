namespace Walrus

open System

type Row =
    {
        Values : obj[]
    }

module Row =

    let ofValues values =
        { Values = values }

    let getValue<'t> column row =
        let value = row.Values[column.Index]
        if isNull value then None
        else Some (value :?> 't)

    let ofStrings columnTypes strings =
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
            |> ofValues
