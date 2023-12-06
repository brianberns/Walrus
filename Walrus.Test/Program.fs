namespace Walrus.Test

open Walrus

module Program =

    Csv.loadTable @"C:\Users\brian\Downloads\train.csv"
        |> Table.pivot
            (col<int> "Pclass")
            (col<int> "Survived")
            (col<int> "PassengerId")
            Seq.length
            (function
                | Some 0 -> "Died"
                | Some 1 -> "Survived"
                | value -> string value)
        |> Table.orderBy "Pclass"
        |> Table.print
