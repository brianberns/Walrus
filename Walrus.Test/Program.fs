namespace Walrus.Test

open Walrus

module Program =

    [<EntryPoint>]
    let main argv =

        let byClass =
            Table.loadCsv "titanic.csv"
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
            ] |> Table.print

        0
