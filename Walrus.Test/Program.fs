namespace Walrus.Test

open Walrus

module Program =

    Table.readCsv @"C:\Users\brian\Downloads\train.csv"
        |> Table.print
