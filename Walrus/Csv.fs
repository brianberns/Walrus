namespace Walrus

open System
open System.Globalization
open System.IO

open CsvHelper

module internal Option =

    /// Boxes the contained value.
    let box opt =
        opt
            |> Option.map box
            |> Option.defaultValue null

module internal Csv =

    /// Supported data types.
    type private ColumnType =
        | Boolean
        | Float
        | Integer
        | String

    /// Parser for each supported data type.
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

    /// Data type preferences. E.g. Prefer to parse "1" as a
    /// boolean, rather than a number or a string.
    let private colTypePriority =
        [|
            Boolean   // highest priority
            Integer
            Float
            String    // lowest priority
        |]

    /// Infers a data type for each colummn in the given lines.
    let private inferTypes nCols lines =

        let colTypeSets =
            Array.replicate nCols (set parserMap.Keys)

        (colTypeSets, lines)
            ||> Seq.fold (fun acc (line : string[]) ->
                [|
                    for iCol = 0 to nCols - 1 do
                        acc[iCol]
                            |> Set.filter (fun colType ->
                                let value = line[iCol]
                                if String.IsNullOrEmpty(value) then true
                                else
                                    parserMap[colType] line[iCol]
                                        |> isNull
                                        |> not)
                |])
            |> Seq.map (fun colTypes ->
                colTypePriority
                    |> Seq.find (fun ct -> colTypes.Contains(ct)))
            |> Seq.toArray

    /// Parses a row of values of the given types from the given
    /// strings.
    let private createRow columnTypes strings =
        Array.zip columnTypes strings
            |> Array.map (fun (colType, str) ->
                parserMap[colType] str)

    /// Loads the given CSV.
    let private loadReader (reader : TextReader) =

        let headers, lines =

            use reader = new CsvReader(reader, CultureInfo.InvariantCulture)

            let flag = reader.Read()
            assert(flag)

            let flag = reader.ReadHeader()
            assert(flag)

            let lines =
                [|
                    while reader.Read() do
                        reader.Parser.Record
                |]

            reader.HeaderRecord, lines

        let colTypes = inferTypes headers.Length lines
        let rows =
            [|
                for line in lines do
                    createRow colTypes line
            |]
        headers, rows

    /// Loads the given CSV file.
    let loadFile path =
        use reader = new StreamReader(path : string)
        loadReader reader

    /// Loads the given CSV string.
    let loadString string =
        use reader = new StringReader(string)
        loadReader reader
