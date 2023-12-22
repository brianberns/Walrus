module GroupBy

open Xunit
open Walrus

// Deedle calls this "Can aggregate rows by key pairs with missing item in pairs".
[<Fact>]
let ``Group by`` () =

    let table =
        [
            [ box 10.; 1.; 1. ]
            [    null; 2.; 2. ]
            [ box 10.; 1.; 3. ]
            [    null; 2.; 4. ]
            [ box 10.; 3.; 5. ]
        ] |> Table.ofRows [ "A"; "B"; "C" ]

    let expected =
        [
            [ box 10.; 1.; 2. ]
            [    null; 2.; 3. ]
            [ box 10.; 3.; 5. ]
        ] |> Table.ofRows [ "A"; "B"; "C" ]

    let actual =
        table
            |> Table.groupBy ["A"; "B"] ["C"] (
                Seq.choose<_, float> id >> Seq.average)

    Assert.Equal(expected, actual)
