namespace Walrus

/// A typed column of values.
type Column<'t> =
    {
        /// Values in this column.
        Values : 't[]
    }

    static member inline (+)(columnA : Column<_>, columnB : Column<_>) =
        { Values = Array.map2 (+) columnA.Values columnB.Values }

    static member inline (-)(columnA : Column<_>, columnB : Column<_>) =
        { Values = Array.map2 (-) columnA.Values columnB.Values }

    static member inline (*)(columnA : Column<_>, columnB : Column<_>) =
        { Values = Array.map2 (*) columnA.Values columnB.Values }

    static member inline (/)(columnA : Column<_>, columnB : Column<_>)  =
        { Values = Array.map2 (/) columnA.Values columnB.Values }

    static member inline (+)(column : Column<'u>, value : 'u) =
        { Values = Array.map (fun v -> v + value) column.Values }

    static member inline (-)(column : Column<'u>, value : 'u) =
        { Values = Array.map (fun v -> v - value) column.Values }

    static member inline (*)(column : Column<'u>, value : 'u) =
        { Values = Array.map (fun v -> v * value) column.Values }

    static member inline (/)(column : Column<'u>, value : 'u) =
        { Values = Array.map (fun v -> v / value) column.Values }

    (*
    static member inline (+)(value : 'u, column : Column<'u>) =
        { Values = Array.map (fun v -> value + v) column.Values }

    static member inline (-)(value : 'u, column : Column<'u>) =
        { Values = Array.map (fun v -> value - v) column.Values }

    static member inline (*)(value : 'u, column : Column<'u>) =
        { Values = Array.map (fun v -> value * v) column.Values }

    static member inline (/)(value : 'u, column : Column<'u>) =
        { Values = Array.map (fun v -> value / v) column.Values }
    *)

module Column =

    /// Gets the value in the given row of the given column.
    let getValue iRow column =
        column.Values[iRow]

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

    /// Rounds the values in the given column.
    static member Round(column : Column<_>) =
        Column.map round column
