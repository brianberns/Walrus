namespace Walrus

type Column<'t> =
    {
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

    let getValue iRow column =
        column.Values[iRow]

    let create values =
        {
            Values = Seq.toArray values
        }

    let replicate count initial =
        Seq.replicate count initial
            |> create

    let map mapping column =
        column.Values
            |> Seq.map mapping
            |> create

type Column<'t> with

    static member inline Round(column : Column<_>) =
        Column.map round column
