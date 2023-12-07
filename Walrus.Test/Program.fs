namespace Walrus.Test

open Walrus

module Program =

    [<EntryPoint>]
    let main argv =

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
            ] |> Table.print

        0
