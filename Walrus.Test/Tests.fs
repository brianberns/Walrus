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
            ]
                |> Seq.map Row.create
                |> Table.create [ "Died (%)"; "Survived (%)" ]

        let actual =
            Csv.loadTable "titanic.csv"
                |> Table.pivot
                    (col<int> "Pclass")
                    (col<bool> "Survived")
                    (col<int> "PassengerId")
                    Seq.length
                    (function
                        | Some false -> "Died"
                        | Some true -> "Survived"
                        | value -> string value)
                |> Table.orderBy "Pclass"
                |> Table.mapRows
                    [
                        "Died (%)", (fun row table ->
                            let died = Table.getValue<int> "Died" row table
                            let survived = Table.getValue<int> "Survived" row table
                            round (100.0 * float died / float (died + survived)))
                        "Survived (%)", (fun row table ->
                            let died = Table.getValue<int> "Died" row table
                            let survived = Table.getValue<int> "Survived" row table
                            round (100.0 * float survived / float (died + survived)))
                    ]

        Assert.AreEqual<_>(expected, actual)
