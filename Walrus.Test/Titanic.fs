module Titanic

open Xunit
open Walrus

let titanic = Table.loadCsvFile "titanic.csv"

/// https://fslab.org/Deedle/index.html
[<Fact>]
let ``Survival by class`` () =
        
    let expected =
        [
            [ 1.; 37.; 63. ]
            [ 2.; 53.; 47. ]
            [ 3.; 76.; 24. ]
        ] |> Table.ofRows [ "Passenger class"; "Died (%)"; "Survived (%)" ]

    let actual =
        let byClass =
            titanic
                |> Table.pivot ["Pclass"] "Survived"
                |> Table.sortRowsBy ["Pclass"]
                |> Table.renameColumns [ "Pclass"; "Died"; "Survived" ]
        let byClass =
            byClass?Died + byClass?Survived
                |> Table.ofColumn "Total"
                |> Table.unionColumns byClass
        Table.ofColumns
            [
                "Passenger class", byClass?Pclass
                "Died (%)", round (byClass?Died / byClass?Total * 100.)
                "Survived (%)", round (byClass?Survived / byClass?Total * 100.)
            ]

    Assert.Equal(expected, actual)

[<Fact>]
let ``Survival by sex`` () =

    do
        let expected =
            [
                [ box "male"; 468; 109 ]
                [ box "female"; 81; 233 ]
            ] |> Table.ofRows [ "Sex"; "False"; "True" ]
        let actual =
            titanic
                |> Table.pivot ["Sex"] "Survived"
        Assert.Equal(expected, actual)

    do
        let expected =
            [
                [ box "male"; 32.; 27. ]
                [ box "female"; 25.; 29. ]
            ] |> Table.ofRows [ "Sex"; "False"; "True" ]
        let actual =
            titanic
                |> Table.pivotWith ["Sex"] "Survived" "Age" (
                    Seq.choose id >> Seq.average<float> >> round)
        Assert.Equal(expected, actual)

[<Fact>]
let ``Group by class and port`` () =

    let expected =
        [
            [ box "C"; 1;  26; 59 ]
            [ box "C"; 2;   8;  9 ]
            [ box "C"; 3;  41; 25 ]
            [ box "S"; 1;  53; 74 ]
            [ box "S"; 2;  88; 76 ]
            [ box "S"; 3; 286; 67 ]
        ] |> Table.ofRows [ "Embarked"; "Pclass"; "False"; "True" ]

    let actual =
        titanic
            |> Table.rowsWhere (fun row ->
                    match Row.getValue<string> "Embarked" row with
                        | "C" | "S" -> true
                        | _ -> false)
            |> Table.pivot [ "Embarked"; "Pclass" ] "Survived"
            |> Table.sortRowsBy [ "Embarked"; "Pclass" ]

    Assert.Equal(expected, actual)

[<Fact>]
let Slice () =
    Assert.Equal(4, titanic["Pclass" .. "Age"].ColumnCount)
    Assert.Equal(4, titanic[2 .. 5].ColumnCount)
    Assert.Equal(2, titanic["Cabin" .. ].ColumnCount)
    Assert.Equal(0, titanic["Embarked" .. "PassengerId"].ColumnCount)

[<Fact>]
let Distinct () =
    let distinct =
        titanic
            |> Table.slice [ "Survived"; "Sex" ]
            |> Table.distinct
    Assert.Equal(4, distinct.RowCount)
