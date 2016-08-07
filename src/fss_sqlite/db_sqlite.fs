namespace Fss.Data

/// SQLite database wrapper.  Independent of other Fss pieces,
/// can be omitted along with System.Data and SQLite dependencies for a smaller compilation unit
/// or used standalone from other Fss pieces.
/// Credit to Thomas Petricek for the original dynamic operator concept 
open System.IO
open System
open System.Data
open System.Data.SQLite


module SQLite = 

    type SQLiteCustomizations() =  class
        interface Fss.Data.Common.Customization<SQLiteParameter,SQLiteConnection> with
            member x.registerEnum<'Enum when 'Enum:(new:unit->'Enum) and 'Enum:struct and 'Enum :> System.ValueType>() =
                failwith "ERROR: unimplemented register enums for sqlite"
                
            member x.reloadTypes (_:SQLiteConnection) =
                ()
            member x.reopenConnection(conn:SQLiteConnection) = 
                SQLiteConnection.ClearPool(conn)

            /// how does sqlite retrieve the last serial value for an insert?
            member x.sequenceMechanism() = Fss.Data.Common.SQL_STMT("select last_insert_rowid()")
            member x.needsKeepAlive() = false
            member x.loadColDetail(conn:SQLiteConnection) =
                let schema = conn.GetSchema("Columns")
                let tableCol = schema.Columns.["TABLE_NAME"].Ordinal
                let colCol = schema.Columns.["COLUMN_NAME"].Ordinal
                let dataTypeCol = schema.Columns.["DATA_TYPE"].Ordinal
                let ordinalPositionCol = schema.Columns.["ORDINAL_POSITION"].Ordinal
                let isNullableCol = schema.Columns.["IS_NULLABLE"].Ordinal
                let isPK = schema.Columns.["PRIMARY_KEY"].Ordinal
                let cols = [|
                                    for row in schema.Rows ->
                                        {   isEnum = false
                                            relName = (row.[tableCol] :?> string).ToLower()
                                            cname = row.[colCol] :?> string
                                            ctype = row.[dataTypeCol] :?> string
                                            cpos =  row.[ordinalPositionCol] :?> int32 |> int16
                                            cNotNull = (row.[isNullableCol] :?> bool) |> not
                                            isPK= (row.[isPK] :?> bool) 
                                            typType = 'x'
                                        } : Fss.Data.Common.ColDetail
                                    |] |> Array.groupBy (fun cd -> cd.relName)
                                    |> Array.map (fun (name,cols) -> name,Array.ofSeq cols)
                                    |> Map.ofArray
                cols
               
                
    end

    type ISqlConnection = Fss.Data.Common.ISqlConnection
    type DynamicSqlConnection = Fss.Data.Common.DynamicSqlConnection<SQLiteConnection,SQLiteParameter,SQLiteCustomizations>
    type DynamicSqlTransaction = Fss.Data.Common.DynamicSqlTransaction<SQLiteParameter,SQLiteConnection,SQLiteCustomizations>

