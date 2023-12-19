module Program

open Walrus

Join.tableA
    |> Table.mapColumn "ValueA" (fun x -> x * 2)
    |> Table.print
