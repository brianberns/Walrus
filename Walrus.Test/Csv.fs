module Csv

open System
open Xunit
open Walrus

/// https://theeventscalendar.com/knowledgebase/csv-file-examples-for-importing/
let events = Table.loadCsvFile "CSV-file-for-example.csv"

[<Fact>]
let ``Start date`` () =
    let column =
        Table.getColumn<DateOnly> "EVENT START DATE" events
    let expected = DateOnly.Parse("6/25/2022")
    let actual = column.Values[0]
    Assert.Equal(expected, actual)

[<Fact>]
let ``Start time`` () =
    let column =
        Table.tryGetColumn<TimeOnly> "EVENT START TIME" events
    let expected = TimeOnly.Parse("3:30pm") |> Some
    let actual = column.Values[0]
    Assert.Equal(expected, actual)
    Assert.Equal(None, column.Values[3])

[<Fact>]
let ``All day event`` () =
    let column =
        Table.getColumn<bool> "ALL DAY EVENT" events
    Assert.False(column.Values[0])
    Assert.True(column.Values[3])
