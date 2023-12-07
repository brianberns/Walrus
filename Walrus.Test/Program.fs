namespace Walrus.Test

open Walrus

module Program =

    let byClass =
        Csv.loadTable "titanic.csv"
            |> Table.pivot<int, _> "Pclass" "Survived" "PassengerId" Seq.length
            |> Table.sortRowsBy "Pclass"
            |> Table.withColumnNames [ "Pclass"; "Died"; "Survived" ]
    byClass?Died + byClass?Survived
        |> Table.ofColumn "Total"
        |> Table.unionColumns byClass
        (*
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
        *)
        |> Table.print
