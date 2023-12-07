namespace Walrus.Test

open Walrus

module Program =

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
                "Died (%)", (fun row ->
                    let died = Row.getValue<int> "Died" row
                    let survived = Row.getValue<int> "Survived" row
                    round (100.0 * float died / float (died + survived)))
                "Survived (%)", (fun row ->
                    let died = Row.getValue<int> "Died" row
                    let survived = Row.getValue<int> "Survived" row
                    round (100.0 * float survived / float (died + survived)))
            ]
        |> Table.print
