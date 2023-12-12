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

(*
 | KeyA | ValueA | KeyB | ValueB |
 | ---- | ------ | ---- | ------ |
 |    0 |   1000 |    0 |   2000 |
 |    2 |   1002 |    2 |   2002 |
 |    4 |   1004 |    4 |   2004 |
 |    6 |   1006 |    6 |   2006 |
 |    8 |   1008 |    8 |   2008 |
 |   10 |   1010 |   10 |   2010 |
 *)
[<Fact>]
let ``Inner join`` () =
    let expected =
        [ 0 .. 2 .. n ]
            |> Seq.map (fun key ->
                [ box key; box (1000 + key); key; box (2000 + key) ])
            |> Table.ofRows [ "KeyA"; "ValueA"; "KeyB"; "ValueB" ]
    let actual =
        Table.innerJoin (tableA, "KeyA") (tableB, "KeyB")
    Assert.Equal(expected, actual)

    let expected' = Table.getColumn<int> "ValueB" expected
    let actual' = Table.getColumn<int> "ValueB" actual
    Assert.Equal(expected', actual')

(*
 | KeyA | ValueA | KeyB | ValueB |
 | ---- | ------ | ---- | ------ |
 |    0 |   1000 |    0 |   2000 |
 |    1 |   1001 |      |        |
 |    2 |   1002 |    2 |   2002 |
 |    3 |   1003 |      |        |
 |    4 |   1004 |    4 |   2004 |
 |    5 |   1005 |      |        |
 |    6 |   1006 |    6 |   2006 |
 |    7 |   1007 |      |        |
 |    8 |   1008 |    8 |   2008 |
 |    9 |   1009 |      |        |
 |   10 |   1010 |   10 |   2010 |
 *)
[<Fact>]
let ``Left join`` () =

    let expected =
        [ 0 .. n ]
            |> Seq.map (fun key ->
                let keyB, valueB =
                    if key % 2 = 0 then box key, box (2000 + key)
                    else null, null
                [ box key; box (1000 + key); keyB; valueB ])
            |> Table.ofRows [ "KeyA"; "ValueA"; "KeyB"; "ValueB" ]
    let actual =
        Table.leftJoin (tableA, "KeyA") (tableB, "KeyB")
    Assert.Equal(expected, actual)

    let expected' = Table.tryGetColumn<int> "ValueB" expected
    let actual' = Table.tryGetColumn<int> "ValueB" actual
    Assert.Equal(expected', actual')
