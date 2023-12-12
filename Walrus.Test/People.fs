module People

open Xunit
open Walrus

/// https://fslab.org/Deedle/frame.html#Loading-F-records-or-NET-objects
type Person =
    { Name:string; Age:int; Countries:string list; }

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
    Assert.Equal<int>(expected, actual)

    let actual =
        people.Rows
            |> Seq.map (fun row ->
                row.GetValue<List<string>>("Countries").Length)
    Assert.Equal<int>(expected, actual)

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
