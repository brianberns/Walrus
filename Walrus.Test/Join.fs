namespace Walrus.Test

open Xunit
open Walrus

module Join =

    [<Fact>]
    let ``Inner join`` () =
        let n = 10
        let expected =
            [ 0 .. 2 .. n ]
                |> Seq.map (fun key ->
                    [ box key; box (1000 + key); box (2000 + key) ])
                |> Table.ofRows [ "KeyA"; "ValueA"; "ValueB" ]
        let actual =
            let tableA =
                [ 0 .. n ]
                    |> Seq.map (fun key -> [ key; 1000 + key ])
                    |> Table.ofRows [ "KeyA"; "ValueA" ]
            let tableB =
                [ 0 .. 2 .. 2 * n ]
                    |> Seq.map (fun key -> [ key; 2000 + key ])
                    |> Table.ofRows [ "KeyB"; "ValueB" ]
            Table.innerJoin (tableA, "KeyA") (tableB, "KeyB")
        Assert.Equal(expected, actual)

        let expected' = Table.getColumn<int> "ValueB" expected
        let actual' = Table.getColumn<int> "ValueB" actual
        Assert.Equal(expected', actual')

    [<Fact>]
    let ``Left join`` () =

        let n = 10
        let expected =
            [ 0 .. n ]
                |> Seq.map (fun key ->
                    let valueB =
                        if key % 2 = 0 then box (2000 + key)
                        else null
                    [ box key; box (1000 + key); valueB ])
                |> Table.ofRows [ "KeyA"; "ValueA"; "ValueB" ]
        let actual =
            let tableA =
                [ 0 .. n ]
                    |> Seq.map (fun key -> [ key; 1000 + key ])
                    |> Table.ofRows [ "KeyA"; "ValueA" ]
            let tableB =
                [ 0 .. 2 .. 2 * n ]
                    |> Seq.map (fun key -> [ key; 2000 + key ])
                    |> Table.ofRows [ "KeyB"; "ValueB" ]
            Table.leftJoin (tableA, "KeyA") (tableB, "KeyB")
        Assert.Equal(expected, actual)

        let expected' = Table.tryGetColumn<int> "ValueB" expected
        let actual' = Table.tryGetColumn<int> "ValueB" actual
        Assert.Equal(expected', actual')
