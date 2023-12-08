namespace Walrus.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Walrus

[<TestClass>]
type Titanic() =

    /// https://fslab.org/Deedle/index.html
    [<TestMethod>]
    member _.Test() =

        let expected =
            [
                [ 1.; 37.; 63. ]
                [ 2.; 53.; 47. ]
                [ 3.; 76.; 24. ]
            ] |> Table.ofRows [ "Passenger class"; "Died (%)"; "Survived (%)" ]

        let actual =
            let byClass =
                Table.loadCsv "titanic.csv"
                    |> Table.pivot "Pclass" "Survived" "PassengerId" Seq.length<int option>
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

    /// https://fslab.org/Deedle/index.html
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
