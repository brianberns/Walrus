module ``Time series``

open System
open Xunit
open Walrus

// Deedle calls this "Can zip-align frames with inner-join left-join nearest-smaller options".
[<Fact>]
let ``Market capitalization`` () =

    let prices =
        [
            [ box (DateOnly(2013, 9, 10)); 100.0; 200.0 ]
            [ box (DateOnly(2013, 9, 11)); 101.0; 200.0 ]
            [ box (DateOnly(2013, 9, 12)); 101.0; 200.0 ]
            [ box (DateOnly(2013, 9, 13)); 101.0; 201.0 ]
            [ box (DateOnly(2013, 9, 14)); 102.0; 101.0 ]
            [ box (DateOnly(2013, 9, 15)); 103.0; 101.5 ]
            [ box (DateOnly(2013, 9, 16)); 104.0; 102.0 ]
        ] |> Table.ofRows [ "Price Date"; "PriceA"; "PriceB" ]

    let shares =
        [
            [ box (DateOnly(2012, 12, 31)); 10.0; 20.0 ]
            [ box (DateOnly(2013,  9, 14)); null; 40.0 ]
        ] |> Table.ofRows [ "Shares Date"; "SharesA"; "SharesB" ]

    let mapFoldColumn colName table =
        Table.mapFoldColumn colName (fun acc factor ->
            let acc' =
                if isNull factor then acc
                else factor
            acc', acc') null table
            |> fst

    let dense =
        Table.outerJoin (prices, "Price Date") (shares, "Shares Date")
            |> Table.sortRowsBy [ "Price Date" ; "Shares Date" ]
            |> mapFoldColumn "SharesA"
            |> mapFoldColumn "SharesB"
            |> Table.rowsWhere (Row.getValue "Price Date" >> isNull >> not)

    let mktcapA =
        dense?PriceA * dense?SharesA
    let expectedA =
        Column.create [
            1000.0
            1010.0
            1010.0
            1010.0
            1020.0
            1030.0
            1040.0
        ]
    Assert.Equal(expectedA, mktcapA)

    let mktcapB =
        dense?PriceB * dense?SharesB
    let expectedB =
        Column.create [
            4000.0
            4000.0
            4000.0
            4020.0
            4040.0
            4060.0
            4080.0
        ]
    Assert.Equal(expectedB, mktcapB)
