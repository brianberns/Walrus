namespace Walrus.Test

open Xunit
open Walrus

module Titanic =

    /// https://fslab.org/Deedle/index.html
    [<Fact>]
    let ``Survival by class`` () =
        
        let expected =
            [
                [ 1.; 37.; 63. ]
                [ 2.; 53.; 47. ]
                [ 3.; 76.; 24. ]
            ] |> Table.ofRows [ "Passenger class"; "Died (%)"; "Survived (%)" ]

        let actual =
            let byClass =
                Table.loadCsv "titanic.csv"
                    |> Table.pivot ["Pclass"] "Survived"
                    |> Table.sortRowsBy ["Pclass"]
                    |> Table.renameColumns [ "Pclass"; "Died"; "Survived" ]
            let byClass =
                byClass?Died + byClass?Survived
                    |> Table.ofColumn "Total"
                    |> Table.unionColumns byClass
            Table.ofColumns
                [
                    "Passenger class", byClass?Pclass
                    "Died (%)", round (byClass?Died / byClass?Total * 100.)
                    "Survived (%)", round (byClass?Survived / byClass?Total * 100.)
                ]

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Survival by sex`` () =

        do
            let expected =
                [
                    [ box "male"; 468; 109 ]
                    [ box "female"; 81; 233 ]
                ] |> Table.ofRows [ "Sex"; "False"; "True" ]

            let actual =
                Table.loadCsv "titanic.csv"
                    |> Table.pivot ["Sex"] "Survived"

            Assert.Equal(expected, actual)

        do
            let expected =
                [
                    [ box "male"; 32.; 27. ]
                    [ box "female"; 25.; 29. ]
                ] |> Table.ofRows [ "Sex"; "False"; "True" ]

            let actual =
                Table.loadCsv "titanic.csv"
                    |> Table.pivotWith ["Sex"] "Survived" "Age" (
                        Seq.choose id >> Seq.average<float> >> round)

            Assert.Equal(expected, actual)

    [<Fact>]
    let ``Group by class and port`` () =

        let expected =
            [
                [ box "C"; 1;  26; 59 ]
                [ box "C"; 2;   8;  9 ]
                [ box "C"; 3;  41; 25 ]
                [ box "S"; 1;  53; 74 ]
                [ box "S"; 2;  88; 76 ]
                [ box "S"; 3; 286; 67 ]
            ] |> Table.ofRows [ "Embarked"; "Pclass"; "False"; "True" ]

        let actual =
            Table.loadCsv "titanic.csv"
                |> Table.rowsWhere (fun row ->
                        match Row.getValue<string> "Embarked" row with
                            | "C" | "S" -> true
                            | _ -> false)
                |> Table.pivot [ "Embarked"; "Pclass" ] "Survived"
                |> Table.sortRowsBy [ "Embarked"; "Pclass" ]

        Assert.Equal(expected, actual)

type Person = 
    { Name:string; Age:int; Countries:string list; }

/// https://fslab.org/Deedle/frame.html#Loading-F-records-or-NET-objects
module People =

    let peopleRecds = 
      [ { Name = "Joe"; Age = 51; Countries = [ "UK"; "US"; "UK"] }
        { Name = "Tomas"; Age = 28; Countries = [ "CZ"; "UK"; "US"; "CZ" ] }
        { Name = "Eve"; Age = 2; Countries = [ "FR" ] }
        { Name = "Suzanne"; Age = 15; Countries = [ "US" ] } ]

    let people = Table.ofRecords peopleRecds

    [<Fact>]
    let ``Country counts`` () =

        let expected = [ 3; 4; 1; 1 ]

        let actual =
            people
                |> Table.getColumn<List<string>> "Countries"
                |> Column.map List.length
                |> Column.values
        Assert.Equal<seq<_>>(expected, actual)

        let actual =
            people.Rows
                |> Seq.map (fun row ->
                    row.GetValue<List<string>>("Countries").Length)
        Assert.Equal<seq<_>>(expected, actual)

    [<Fact>]
    let ``Union rows`` () =
        let jimTable =
            [ { Name = "Jim"; Age = 51; Countries = ["US"] } ]
                |> Table.ofRecords
        let union = Table.unionRows people jimTable
        Assert.Equal(5, Seq.length union.Rows)

    /// More realistic than the Deedle example.
    [<Fact>]
    let Travels () =
        let travels =
            seq {
                for person in peopleRecds do
                    for country in person.Countries do
                        yield [ person.Name; country ]
            }
                |> Table.ofRows [ "Name"; "Country" ]
                |> Table.pivot ["Name"] "Country"
        let joe =
            travels.Rows
                |> Seq.find (Row.getValue "Name" >> (=) "Joe")
        Assert.Equal(0, Row.getValue "CZ" joe)
        Assert.Equal(2, Row.getValue "UK" joe)
        Assert.Equal(1, Row.getValue "US" joe)

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
