module Program

open Walrus

let init =
    Table.loadCsvFile "titanic.csv"            // load Titanic data from a CSV file
        |> Table.pivot ["Pclass"] "Survived"   // count the # of survivors in each passenger class
        |> Table.sortRowsBy ["Pclass"]         // sort the resulting pivot table's rows
        |> Table.renameColumns                 // give each column a meaningful name
            [ "Pclass"; "Died"; "Survived" ]
let byClass =                                  // add a column for the # of passengers in each class
    init?Died + init?Survived
        |> Table.ofColumn "Total"
        |> Table.unionColumns init
Table.ofColumns                                // compute percentages instead of raw counts
    [
        "Passenger class", byClass?Pclass
        "Died (%)", round (byClass?Died / byClass?Total * 100.)
        "Survived (%)", round (byClass?Survived / byClass?Total * 100.)
    ] |> Table.print                           // print the result
