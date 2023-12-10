# Walrus

Walrus is a lightweight F# library for working with tabular data. It's similar to [Deedle](https://fslab.org/Deedle), but much simpler to use.

## Tables, rows, and columns

A Walrus `Table` is a sequence of `Row`s that can be accessed by column name. It's also possible to access an entire column of strongly-typed values at once.

## Example

Here's Deedle's Titanic survivor analysis in Walrus:

```fsharp
let byClass =
    Table.loadCsv "titanic.csv"                // load Titanic data from a CSV file
        |> Table.pivot ["Pclass"] "Survived"   // count the # of survivors in each passenger class
        |> Table.sortRowsBy ["Pclass"]         // sort the resulting pivot table's rows
        |> Table.renameColumns                 // give each column a meaningful name
            [ "Pclass"; "Died"; "Survived" ]
let byClass =                                  // add a column for the # of passengers in each class
    byClass?Died + byClass?Survived
        |> Table.ofColumn "Total"
        |> Table.unionColumns byClass
Table.ofColumns                                // compute percentages instead of raw counts
    [
        "Passenger class", byClass?Pclass
        "Died (%)", round (byClass?Died / byClass?Total * 100.)
        "Survived (%)", round (byClass?Survived / byClass?Total * 100.)
    ] |> Table.print                           // print the result
```

Output:

```
 | Passenger class | Died (%) | Survived (%) |
 | --------------- | -------- | ------------ |
 |               1 |       37 |           63 |
 |               2 |       53 |           47 |
 |               3 |       76 |           24 |
```