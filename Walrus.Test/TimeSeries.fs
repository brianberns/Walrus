module TimeSeries

open System
open Xunit
open Walrus

let pxA =
    [
        DateOnly(2013, 9, 10), 100.0
        DateOnly(2013, 9, 11), 101.0
        DateOnly(2013, 9, 12), 101.0
        DateOnly(2013, 9, 13), 101.0
        DateOnly(2013, 9, 14), 102.0
        DateOnly(2013, 9, 15), 103.0
        DateOnly(2013, 9, 16), 104.0
    ]
        |> Seq.map (fun (dt, price) -> [box dt; price])
        |> Table.ofRows [ "Price Date"; "Price" ]

let sharesA =
    [
        DateOnly(2012, 12, 31), 10.0
    ]
        |> Seq.map (fun (dt, factor) -> [box dt; factor])
        |> Table.ofRows [ "Factor Date"; "Factor" ]

// Deedle calls this "Can zip-align frames with inner-join left-join nearest-smaller options".
[<Fact>]
let ``Market capitalization`` () =

    let dense =
        Table.outerJoin (pxA, "Price Date") (sharesA, "Factor Date")
            |> Table.sortRowsBy [ "Price Date" ; "Factor Date" ]
            |> Table.mapFoldColumn "Factor" (fun acc factor ->
                let acc' =
                    if isNull factor then acc
                    else factor
                acc', acc') null
            |> fst
            |> Table.rowsWhere (Row.getValue "Price Date" >> isNull >> not)
    let mktcapA =
        dense?Price * dense?Factor
            |> Table.ofColumn "Total"
            |> Table.unionColumns dense
    let expected =
        Column.create [
            1000.0
            1010.0
            1010.0
            1010.0
            1020.0
            1030.0
            1040.0
        ]
    let actual = mktcapA?Total
    Assert.Equal(expected, actual)
