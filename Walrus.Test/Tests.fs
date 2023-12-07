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
            Csv.loadTable "titanic.csv"
                |> Table.pivot<int, _> "Pclass" "Survived" "PassengerId" Seq.length
                |> Table.sortRowsBy "Pclass"
                |> Table.withColumnNames [ "Pclass"; "Died"; "Survived" ]
                |> Table.mapRows
                    [
                        "Died (%)", (fun row ->
                            let died = Row.getValue<int> "Died" row
                            let survived = Row.getValue<int> "Survived" row
                            round (100.0 * float died / float (died + survived)))
                        "Survived (%)", (fun row ->
                            let died = Row.getValue<int> "Died" row
                            let survived = Row.getValue<int> "Survived" row
                            round (100.0 * float survived / float (died + survived)))
                    ]

        Assert.AreEqual<_>(expected, actual)
