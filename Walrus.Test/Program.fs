namespace Walrus.Test

open Walrus

module Program =

    Csv.loadTable @"C:\Users\brian\Downloads\train.csv"
        |> Table.pivot
            (col<int> "Pclass")
            (col<int> "Survived")
            (col<int> "PassengerId")
            Seq.length
        |> Table.print
        // |> printfn "%A"
