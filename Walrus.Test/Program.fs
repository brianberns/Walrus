module Program

open Walrus

Table.rightJoin (Join.tableA, "KeyA") (Join.tableB, "KeyB")
    |> Table.print
