namespace Fss.Data

/// SQLite database wrapper.  Independent of other Fss pieces,
/// can be omitted along with System.Data and SQLite dependencies for a smaller compilation unit
/// or used standalone from other Fss pieces.
/// Credit to Thomas Petricek for the original dynamic operator concept 
open System.IO
open System
open System.Data
open Microsoft.Data.Sqlite


module SQLite = 

    type SQLiteCustomizations() =  class
        interface Fss.Data.Common.Customization<SqliteParameter,SqliteConnection> with
            member x.reloadTypes (_:SqliteConnection) =
                ()
            member x.reopenConnection(conn:SqliteConnection) = 
                SqliteConnection.ClearPool(conn)

            member x.getSearchPath(_:SqliteConnection) = ["main"]

            /// how does sqlite retrieve the last serial value for an insert?
            member x.sequenceMechanism() = Fss.Data.Common.SQL_STMT("select last_insert_rowid()")
            member x.needsKeepAlive() = false
            member x.loadColDetail(conn:SqliteConnection) =
                let schema = conn.GetSchema("Columns")
                let schemaCol = schema.Columns.["TABLE_SCHEMA"].Ordinal
                let tableCol = schema.Columns.["TABLE_NAME"].Ordinal
                let colCol = schema.Columns.["COLUMN_NAME"].Ordinal
                let dataTypeCol = schema.Columns.["DATA_TYPE"].Ordinal
                let ordinalPositionCol = schema.Columns.["ORDINAL_POSITION"].Ordinal
                let isNullableCol = schema.Columns.["IS_NULLABLE"].Ordinal
                let isPK = schema.Columns.["PRIMARY_KEY"].Ordinal
                let cols = [|
                                    for row in schema.Rows ->
                                        {   schema = (row.[schemaCol] :?> string).ToLower()
                                            isEnum = false
                                            relName = (row.[tableCol] :?> string).ToLower()
                                            cname = row.[colCol] :?> string
                                            ctype = row.[dataTypeCol] :?> string
                                            cpos =  System.Convert.ToInt16(row.[ordinalPositionCol])
                                            cNotNull = (row.[isNullableCol] :?> bool) |> not
                                            isPK= (row.[isPK] :?> bool) 
                                            typType = 'x'
                                        } : Fss.Data.Common.ColDetail
                                    |] |> Array.groupBy (fun cd -> (cd.schema,cd.relName))
                                    |> Array.map (fun ((schema,name),cols) -> ({schema = schema ; table = name}:Fss.Data.Common.SchematizedTable),Array.ofSeq cols)
                                    |> Map.ofArray
                cols
               
                
    end

    type ISqlConnection = Fss.Data.Common.ISqlConnection
    type DynamicSqlConnection = Fss.Data.Common.DynamicSqlConnection<SqliteConnection,SqliteParameter,SQLiteCustomizations>
    type DynamicSqlTransaction = Fss.Data.Common.DynamicSqlTransaction<SqliteParameter,SqliteConnection,SQLiteCustomizations>

