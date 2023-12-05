namespace Walrus.Test

open Walrus

module Program =

    let getPClass = Table.getValue "PClass" Row.getValue<int>
    let getSurvived = Table.getValue "Survived" Row.getValue<int>
    let getPassengerId = Table.getValue "PassengerId" Row.getValue<int>

    Table.readCsv @"C:\Users\brian\Downloads\train.csv"
        (*
        |> Table.pivot
            getPClass
            getSurvived
            getPassengerId
            Seq.length
        *)
        |> Table.print
