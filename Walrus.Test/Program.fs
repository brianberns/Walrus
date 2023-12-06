namespace Walrus.Test

open Walrus

module Program =

    Csv.loadTable @"C:\Users\brian\Downloads\train.csv"
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
                "Died (%)", (fun table row ->
                    let died = Table.getValue<int> "Died" table row
                    let survived = Table.getValue<int> "Survived" table row
                    round (100.0 * float died / float (died + survived)))
                "Survived (%)", (fun table row ->
                    let died = Table.getValue<int> "Died" table row
                    let survived = Table.getValue<int> "Survived" table row
                    round (100.0 * float survived / float (died + survived)))
            ]
        |> Table.print
