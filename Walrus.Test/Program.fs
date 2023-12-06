namespace Walrus.Test

open Walrus

module Program =

    Csv.loadTable @"C:\Users\brian\Downloads\train.csv"
        |> Table.pivot<int, int> "Pclass" "Survived" "PassengerId" Seq.length
        |> Table.print
