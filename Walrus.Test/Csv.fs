module Csv

open System
open Xunit
open Walrus

/// https://theeventscalendar.com/knowledgebase/csv-file-examples-for-importing/
[<Fact>]
let Events () =

    let events = Table.loadCsvFile "CSV-file-for-example.csv"
    let column =
        Table.getColumn<DateOnly> "EVENT START DATE" events
    let expected = DateOnly.Parse("6/25/2022")
    let actual = column.Values[0]
    Assert.Equal(expected, actual)
