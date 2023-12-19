module Program

open Walrus

Join.tableA
    |> Table.mapColumn "ValueA" "2x" (fun x -> x * 2)
    |> Table.print
