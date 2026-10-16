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
            member x.parameterPrefix() = "@"
            member x.openConnection(connStr, _first) =
                let c = new SqliteConnection(connStr)
                c.Open()
                c
            member x.loadColDetail(conn:SqliteConnection) =
                // Microsoft.Data.Sqlite does not implement GetSchema("Columns") (System.Data.SQLite did),
                // so read column metadata with the table_info pragma for every table and view.
                use command = conn.CreateCommand()
                command.CommandText <- """
                    select m.name as table_name, p.cid, p.name as column_name, p.type, p."notnull", p.pk
                    from sqlite_master m join pragma_table_info(m.name) p
                    where m.type in ('table','view') and m.name not like 'sqlite_%'
                    order by m.name, p.cid"""
                use r = command.ExecuteReader()
                let cols =
                    [|
                        while r.Read() do
                            yield
                                {   schema = "main"
                                    isEnum = false
                                    relName = r.GetString(0).ToLower()
                                    cpos = int16 (r.GetInt64(1) + 1L)
                                    cname = r.GetString(2)
                                    ctype = r.GetString(3)
                                    cNotNull = r.GetInt64(4) <> 0L
                                    isPK = r.GetInt64(5) > 0L
                                    typType = 'x'
                                } : Fss.Data.Common.ColDetail
                    |]
                cols
                |> Array.groupBy (fun cd -> cd.schema, cd.relName)
                |> Array.map (fun ((schema,name),cols) -> ({schema = schema ; table = name}:Fss.Data.Common.SchematizedTable), cols)
                |> Map.ofArray

    end

    type ISqlConnection = Fss.Data.Common.ISqlConnection
    type DynamicSqlConnection = Fss.Data.Common.DynamicSqlConnection<SqliteConnection,SqliteParameter,SQLiteCustomizations>
    type DynamicSqlTransaction = Fss.Data.Common.DynamicSqlTransaction<SqliteParameter,SqliteConnection,SQLiteCustomizations>

