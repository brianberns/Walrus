namespace Walrus.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Walrus

[<TestClass>]
type Titanic() =

    /// https://fslab.org/Deedle/index.html
    [<TestMethod>]
    member _.Survival() =

        let expected =
            [
                [ 1.; 37.; 63. ]
                [ 2.; 53.; 47. ]
                [ 3.; 76.; 24. ]
            ] |> Table.ofRows [ "Passenger class"; "Died (%)"; "Survived (%)" ]

        let actual =
            let byClass =
                Table.loadCsv "titanic.csv"
                    |> Table.pivot "Pclass" "Survived"
                    |> Table.sortRowsBy "Pclass"
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

        Assert.AreEqual<_>(expected, actual)

    [<TestMethod>]
    member _.ByClassAndPort() =
        Table.loadCsv "titanic.csv"
            |> Table.pivotWith "Pclass" "Embarked" "Age" (fun ageOpts ->
                let ages =
                    ageOpts
                        |> Seq.choose id
                        |> Seq.toArray
                if ages.Length = 0 then 0.0
                else Array.average ages |> round)
            |> Table.print

type Person = 
    { Name:string; Age:int; Countries:string list; }

/// https://fslab.org/Deedle/frame.html#Loading-F-records-or-NET-objects
[<TestClass>]
type People() =

    let peopleRecds = 
      [ { Name = "Joe"; Age = 51; Countries = [ "UK"; "US"; "UK"] }
        { Name = "Tomas"; Age = 28; Countries = [ "CZ"; "UK"; "US"; "CZ" ] }
        { Name = "Eve"; Age = 2; Countries = [ "FR" ] }
        { Name = "Suzanne"; Age = 15; Countries = [ "US" ] } ]

    let people = Table.ofRecords peopleRecds

    [<TestMethod>]
    member _.CountryCounts() =

        let expected = [3; 4; 1; 1]

        let actual =
            people
                |> Table.getColumn<List<string>> "Countries"
                |> Column.map List.length
                |> Column.values
                |> Seq.toList
        Assert.AreEqual<_>(expected, actual)

        let actual =
            people.Rows
                |> Seq.map (fun row ->
                    row.GetValue<List<string>>("Countries").Length)
                |> Seq.toList
        Assert.AreEqual<_>(expected, actual)

    [<TestMethod>]
    member _.UnionRows() =
        let jimTable =
            [ { Name = "Jim"; Age = 51; Countries = ["US"] } ]
                |> Table.ofRecords
        let union = Table.unionRows people jimTable
        Assert.AreEqual<_>(5, Seq.length union.Rows)

    /// More realistic than the Deedle example.
    [<TestMethod>]
    member _.Travels() =
        let travels =
            seq {
                for person in peopleRecds do
                    for country in person.Countries do
                        yield [ person.Name; country ]
            }
                |> Table.ofRows [ "Name"; "Country" ]
                |> Table.pivot "Name" "Country"
        let joe =
            travels.Rows
                |> Seq.find (Row.getValue "Name" >> (=) "Joe")
        Assert.AreEqual<_>(0, Row.getValue "CZ" joe)
        Assert.AreEqual<_>(2, Row.getValue "UK" joe)
        Assert.AreEqual<_>(1, Row.getValue "US" joe)
