namespace Fss.Data

open System
open System.Collections
open System.Data
open DuckDB.NET.Data

/// DuckDB database wrapper: map query results onto F# records and insert F# records.
/// Independent of the other Fss pieces (only needs DuckDB.NET).
/// Credit to Thomas Petricek for the original dynamic operator concept

module DuckDB =

    type DuckDBCustomizations() =  class
        interface Fss.Data.Common.Customization<DuckDBParameter,DuckDBConnection> with
            /// DuckDB supports `insert ... returning`
            member x.sequenceMechanism() = Fss.Data.Common.RETURNS_CLAUSE
            /// In-process engine: there is no server connection to drop, so no keep-alive thread
            member x.needsKeepAlive() = false
            /// DuckDB named parameters are written `$name`
            member x.parameterPrefix() = "$"

            /// The first pooled connection opens the database; the others are duplicates of it, so
            /// every pooled connection sees the same database. This matters for `:memory:`, where
            /// opening the connection string again would create a separate, empty database.
            member x.openConnection(connStr, first) =
                let duplicate (f:DuckDBConnection) =
                    // Duplicate() is only supported for in-memory databases. For files, DuckDB.NET
                    // already shares one database instance per path, so a fresh connection is fine.
                    try Some (f.Duplicate()) with :? NotSupportedException -> None
                let c =
                    match first |> Option.bind duplicate with
                    | Some d -> d
                    | None -> new DuckDBConnection(connStr)
                c.Open()
                c

            member x.reloadTypes (_conn:DuckDBConnection) = ()

            member x.reopenConnection(conn:DuckDBConnection) =
                try
                    if conn.State = ConnectionState.Closed then
                        conn.Open()
                with _ ->
                    ()

            member x.getSearchPath(conn:DuckDBConnection) =
                use command = conn.CreateCommand()
                command.CommandText <- "select current_schema(), current_setting('search_path')"
                use r = command.ExecuteReader()
                if r.Read() then
                    let current = r.GetString(0)
                    let extra =
                        if r.IsDBNull(1) then [] else
                        r.GetString(1).Split([|','|], StringSplitOptions.RemoveEmptyEntries)
                        |> Array.map (fun s -> s.Trim())
                        // search_path entries may be qualified as catalog.schema
                        |> Array.map (fun s -> match s.LastIndexOf '.' with -1 -> s | i -> s.Substring(i+1))
                        |> List.ofArray
                    (current :: extra) |> List.map (fun s -> s.ToLower()) |> List.distinct
                else ["main"]

            member x.loadColDetail(conn:DuckDBConnection) =
                // Primary key column names per table, from DuckDB's own catalog
                let pkCols =
                    use comm = conn.CreateCommand()
                    comm.CommandText <- "select schema_name, table_name, constraint_column_names
                                         from duckdb_constraints()
                                         where constraint_type = 'PRIMARY KEY' and database_name = current_database()"
                    use r = comm.ExecuteReader()
                    [ while r.Read() do
                        let names =
                            match r.GetValue(2) with
                            | :? IEnumerable as xs -> [ for n in xs -> (string n).ToLower() ]
                            | _ -> []
                        yield (r.GetString(0).ToLower(), r.GetString(1).ToLower()), Set.ofList names ]
                    |> Map.ofList

                use command = conn.CreateCommand()
                command.CommandText <- "select schema_name, table_name, column_name, column_index, data_type, is_nullable
                                        from duckdb_columns()
                                        where database_name = current_database()
                                        order by schema_name, table_name, column_index"
                use r = command.ExecuteReader()
                [|
                    while r.Read() do
                        let schema = r.GetString(0).ToLower()
                        let table = r.GetString(1).ToLower()
                        let cname = r.GetString(2)
                        let isPK =
                            match pkCols.TryFind((schema,table)) with
                            | Some names -> names.Contains(cname.ToLower())
                            | None -> false
                        yield
                            {   schema = schema
                                relName = table
                                cname = cname
                                cpos = Convert.ToInt16(r.GetValue(3))
                                ctype = r.GetString(4)
                                // PRIMARY KEY columns are implicitly NOT NULL
                                cNotNull = not (r.GetBoolean(5)) || isPK
                                isPK = isPK
                                isEnum = false
                                typType = 'x'
                            } : Fss.Data.Common.ColDetail
                |]
                |> Array.groupBy (fun c -> c.schema, c.relName)
                |> Array.map (fun ((schema,table),cols) -> ({schema = schema ; table = table}:Fss.Data.Common.SchematizedTable), cols)
                |> Map.ofArray
    end

    type ISqlConnection = Fss.Data.Common.ISqlConnection
    type DynamicSqlConnection = Fss.Data.Common.DynamicSqlConnection<DuckDBConnection,DuckDBParameter,DuckDBCustomizations>
    type DynamicSqlTransaction = Fss.Data.Common.DynamicSqlTransaction<DuckDBParameter,DuckDBConnection,DuckDBCustomizations>
