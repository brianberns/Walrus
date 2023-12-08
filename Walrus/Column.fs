namespace Walrus

open System.Numerics

/// A typed column of values.
type Column<'t> =
    {
        /// Values in this column.
        Values : 't[]
    }

module Column =

    /// Gets the value in the given row of the given column.
    let getValue iRow column =
        column.Values[iRow]

    /// All values in the given column.
    let values column =
        column.Values

    /// Creates a column from the given values.
    let create values =
        {
            Values = Seq.toArray values
        }

    /// Creates a new column by applying the given function to
    /// values in the given column.
    let map mapping column =
        column.Values
            |> Seq.map mapping
            |> create

type Column<'t> with

    static member inline (+)(columnA : Column<_>, columnB : Column<_>) =
        Array.map2 (+) columnA.Values columnB.Values |> Column.create

    static member inline (-)(columnA : Column<_>, columnB : Column<_>) =
        Array.map2 (-) columnA.Values columnB.Values |> Column.create

    static member inline (*)(columnA : Column<_>, columnB : Column<_>) =
        Array.map2 (*) columnA.Values columnB.Values |> Column.create

    static member inline (/)(columnA : Column<_>, columnB : Column<_>)  =
        Array.map2 (/) columnA.Values columnB.Values |> Column.create

    static member inline (+)(column : Column<_>, value : #INumber<_>) =
        Array.map (fun v -> v + value) column.Values |> Column.create

    static member inline (-)(column : Column<_>, value : #INumber<_>) =
        Array.map (fun v -> v - value) column.Values |> Column.create

    static member inline (*)(column : Column<_>, value : #INumber<_>) =
        Array.map (fun v -> v * value) column.Values |> Column.create

    static member inline (/)(column : Column<_>, value : #INumber<_>) =
        Array.map (fun v -> v / value) column.Values |> Column.create

    static member inline (+)(value : #INumber<_>, column : Column<_>) =
        Array.map (fun v -> value + v) column.Values |> Column.create

    static member inline (-)(value : #INumber<_>, column : Column<_>) =
        Array.map (fun v -> value - v) column.Values |> Column.create

    static member inline (*)(value : #INumber<_>, column : Column<_>) =
        Array.map (fun v -> value * v) column.Values |> Column.create

    static member inline (/)(value : #INumber<_>, column : Column<_>) =
        Array.map (fun v -> value / v) column.Values |> Column.create

    /// Rounds the values in the given column.
    static member Round(column : Column<_>) =
        Column.map round column
