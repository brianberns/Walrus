module Join

open Xunit
open Walrus

let n = 10

(*
 | KeyA | ValueA |
 | ---- | ------ |
 |    0 |   1000 |
 |    1 |   1001 |
 |    2 |   1002 |
 |    3 |   1003 |
 |    4 |   1004 |
 |    5 |   1005 |
 |    6 |   1006 |
 |    7 |   1007 |
 |    8 |   1008 |
 |    9 |   1009 |
 |   10 |   1010 |
 *)
let tableA =
    [ 0 .. n ]
        |> Seq.map (fun key -> [ key; 1000 + key ])
        |> Table.ofRows [ "KeyA"; "ValueA" ]

(*
 | KeyB | ValueB |
 | ---- | ------ |
 |    0 |   2000 |
 |    2 |   2002 |
 |    4 |   2004 |
 |    6 |   2006 |
 |    8 |   2008 |
 |   10 |   2010 |
 |   12 |   2012 |
 |   14 |   2014 |
 |   16 |   2016 |
 |   18 |   2018 |
 |   20 |   2020 |
 *)
let tableB =
    [ 0 .. 2 .. 2 * n ]
        |> Seq.map (fun key -> [ key; 2000 + key ])
        |> Table.ofRows [ "KeyB"; "ValueB" ]

[<Fact>]
let ``Inner join`` () =

    (*
     | KeyA | ValueA | ValueB |
     | ---- | ------ | ------ |
     |    0 |   1000 |   2000 |
     |    2 |   1002 |   2002 |
     |    4 |   1004 |   2004 |
     |    6 |   1006 |   2006 |
     |    8 |   1008 |   2008 |
     |   10 |   1010 |   2010 |
     *)
    let expected =
        [ 0 .. 2 .. n ]
            |> Seq.map (fun key ->
                [ box key; box (1000 + key); box (2000 + key) ])
            |> Table.ofRows [ "KeyA"; "ValueA"; "ValueB" ]
    let actual =
        Table.innerJoin (tableA, "KeyA") (tableB, "KeyB")
    Assert.Equal(expected, actual)

    let expected' = Table.getColumn<int> "ValueB" expected
    let actual' = Table.getColumn<int> "ValueB" actual
    Assert.Equal(expected', actual')

[<Fact>]
let ``Left join`` () =

    (*
     | KeyA | ValueA | ValueB |
     | ---- | ------ | ------ |
     |    0 |   1000 |   2000 |
     |    1 |   1001 |        |
     |    2 |   1002 |   2002 |
     |    3 |   1003 |        |
     |    4 |   1004 |   2004 |
     |    5 |   1005 |        |
     |    6 |   1006 |   2006 |
     |    7 |   1007 |        |
     |    8 |   1008 |   2008 |
     |    9 |   1009 |        |
     |   10 |   1010 |   2010 |
     *)
    let expected =
        [ 0 .. n ]
            |> Seq.map (fun key ->
                let valueB =
                    if key % 2 = 0 then box (2000 + key)
                    else null
                [ box key; box (1000 + key); valueB ])
            |> Table.ofRows [ "KeyA"; "ValueA"; "ValueB" ]
    let actual =
        Table.leftJoin (tableA, "KeyA") (tableB, "KeyB")
    Assert.Equal(expected, actual)

    let expected' = Table.tryGetColumn<int> "ValueB" expected
    let actual' = Table.tryGetColumn<int> "ValueB" actual
    Assert.Equal(expected', actual')
