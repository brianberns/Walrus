module GroupBy

open Xunit
open Walrus

let input =
    [
        [ box 10.; 1.; 1. ]
        [    null; 2.; 2. ]
        [ box 10.; 1.; 3. ]
        [    null; 2.; 4. ]
        [ box 10.; 3.; 5. ]
    ] |> Table.ofRows [ "A"; "B"; "C" ]

[<Fact>]
let ``TryGetValue`` () =
    let row = Seq.head input.Rows
    Assert.Throws<System.InvalidCastException>(fun () ->
        row.TryGetValue<int> "A"
            |> ignore)
            |> ignore

[<Fact>]
let ``Group by`` () =

    let expected =
        [
            [ box 10.; 1. ], [ 1.; 3. ]
            [    null; 2. ], [ 2.; 4. ]
            [ box 10.; 3. ], [ 5. ]
        ]

    let actual =
        input
            |> Table.groupBy ["A"; "B"]
            |> Seq.map (fun (key, table) ->
                let col = Table.getColumn<float> "C" table
                key, Seq.toList col.Values)
            |> Seq.toList

    Assert.Equal<_ * _>(expected, actual)

[<Fact>]
let ``Group by, strongly typed`` () =

    let expected =
        [
            (Some 10., Some 1.), [ 1.; 3. ]
            (None,     Some 2.), [ 2.; 4. ]
            (Some 10., Some 3.), [ 5. ]
        ]

    let actual =
        input
            |> Table.groupBy2 "A" "B"
            |> Seq.map (fun (key, table) ->
                let col = Table.getColumn<float> "C" table
                key, Seq.toList col.Values)
            |> Seq.toList

    Assert.Equal<_ * _>(expected, actual)

// Deedle calls this "Can aggregate rows by key pairs with missing item in pairs".
[<Fact>]
let ``Aggregate by`` () =

    let expected =
        [
            [ box 10.; 1.; 2. ]
            [    null; 2.; 3. ]
            [ box 10.; 3.; 5. ]
        ] |> Table.ofRows [ "A"; "B"; "C" ]

    let actual =
        input
            |> Table.aggregateBy ["A"; "B"] ["C"] (
                Seq.choose<_, float> id >> Seq.average)

    Assert.Equal(expected, actual)
