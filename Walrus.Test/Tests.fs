namespace Walrus.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Walrus

[<TestClass>]
type TestClass () =

    /// https://fslab.org/Deedle/index.html
    [<TestMethod>]
    member _.Titanic() =

        let expected =
            [
                [ 1.; 37.; 63. ]
                [ 2.; 53.; 47. ]
                [ 3.; 76.; 24. ]
            ] |> Table.ofRows [ "Passenger class"; "Died (%)"; "Survived (%)" ]

        let actual =
            let byClass =
                Table.loadCsv "titanic.csv"
                    |> Table.pivot "Pclass" "Survived" "PassengerId" Seq.length<int option>
                    |> Table.sortRowsBy "Pclass"
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

        Assert.AreEqual<_>(expected, actual)
