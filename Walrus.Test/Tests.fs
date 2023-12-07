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
                [ 37.; 63. ]
                [ 53.; 47. ]
                [ 76.; 24. ]
            ] |> Table.ofRows [ "Died (%)"; "Survived (%)" ]

        let actual =
            let byClass =
                Csv.loadTable "titanic.csv"
                    |> Table.pivot<int, _> "Pclass" "Survived" "PassengerId" Seq.length
                    |> Table.sortRowsBy "Pclass"
                    |> Table.renameColumns [ "Pclass"; "Died"; "Survived" ]
            let byClass =
                byClass?Died + byClass?Survived
                    |> Table.ofColumn "Total"
                    |> Table.unionColumns byClass
            Table.ofColumns
                [
                    "Died (%)", round (byClass?Died / byClass?Total * 100.)
                    "Survived (%)", round (byClass?Survived / byClass?Total * 100.)
                ]

        Assert.AreEqual<_>(expected, actual)
