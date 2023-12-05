namespace Walrus

[<RequireQualifiedAccess>]
type ColumnType =
    | Float
    | Integer
    | String

type Column =
    {
        Name : string
        Type : ColumnType
        Index : int
    }
