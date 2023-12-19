module Program

open Walrus

Table.print Join.tableA
printfn ""

let table, sum =
    Join.tableA
        |> Table.mapFoldColumn
            "ValueA"
            (fun acc state -> acc + state, acc + state)
            0
Table.print table
printfn "%A" sum
