namespace Walrus.Test

open Walrus

module Program =

    [<EntryPoint>]
    let main argv =

        let byClass =
            Table.loadCsv "titanic.csv"                // load Titanic data from a CSV file
                |> Table.pivot ["Pclass"] "Survived"   // count the # of survivors in each passenger class
                |> Table.sortRowsBy ["Pclass"]         // sort the resulting pivot table's rows
                |> Table.renameColumns                 // give each column a meaningful name
                    [ "Pclass"; "Died"; "Survived" ]
        let byClass =                                  // add a column for the # of passengers in each class
            byClass?Died + byClass?Survived
                |> Table.ofColumn "Total"
                |> Table.unionColumns byClass
        Table.ofColumns                                // compute percentages instead of raw counts
            [
                "Passenger class", byClass?Pclass
                "Died (%)", round (byClass?Died / byClass?Total * 100.)
                "Survived (%)", round (byClass?Survived / byClass?Total * 100.)
            ] |> Table.print                           // print the result

        0
