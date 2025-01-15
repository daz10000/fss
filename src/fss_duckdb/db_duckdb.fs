namespace Fss.Data

open System.IO
open DuckDB.NET.Data
open System.Data

/// Duckdb database wrapper.  Independent of other Fss pieces,
/// can be omitted along with System.Data and NPgsql dependencies for a smaller compilation unit
/// or used standalone from other Fss pieces.
/// Credit to Thomas Petricek for the original dynamic operator concept 

module DuckDB = 

    type DuckDBCustomizations() =  class
        interface Fss.Data.Common.Customization<DuckDBParameter,DuckDBConnection> with
            member x.sequenceMechanism() = Fss.Data.Common.RETURNS_CLAUSE
            member x.needsKeepAlive() = true

            member x.reloadTypes (conn:DuckDBConnection) =
                // Problem?
                // conn.ReloadTypes()
                ()
            member x.reopenConnection(conn:DuckDBConnection) = 
                try
                    if conn.State = ConnectionState.Closed then
                        conn.Open()
                with _ ->
                    ()
                    //DuckDBConnection.ClearPool(conn)
            member x.getSearchPath(conn:DuckDBConnection) =
		(*
                use command : DuckDBCommand = conn.CreateCommand()
                command.CommandText <- "show search_path"
                let searchPath = command.ExecuteScalar( ) :?> string

                command.CommandText <- "select CURRENT_USER"
                let currentUser = command.ExecuteScalar() :?> string

                let schemas = searchPath.Split([|','|]) 
                                |> Array.map (fun s -> s.Trim())
                                |> Array.map (fun s -> if s = "$user" then currentUser else s)
                                |> Array.filter (fun x -> not ( 
                                                                x.StartsWith("$") 
                                                              ) 
                                                )
                                |> List.ofArray
                schemas
		*)
		[]

            member x.loadColDetail(conn:DuckDBConnection) =
               


                /// db columns
                let cols:Map<Fss.Data.Common.SchematizedTable,Fss.Data.Common.ColDetail []> =
                        seq { 
                            // Inspect table definition.
                            use command : DuckDBCommand = conn.CreateCommand()
                            command.CommandText <- "select nsp.nspname as namespace,c.relname,t.typtype,a.attname as attname,t.typname as tname,attnum,attnotnull from 
                                                                pg_class c JOIN pg_attribute a ON c.oid = a.attrelid 
                                                                JOIN pg_type t ON t.oid = a.atttypid
                                                                join pg_namespace nsp on nsp.oid = c.relnamespace
                                                                WHERE
                                                                -- c.relname = :tablename AND
                                                                a.attnum > 0"
                            use r = command.ExecuteReader()
                            while r.Read() do
                                let c : Fss.Data.Common.ColDetail = 
                                            {   schema = r.["namespace"] :?> string
                                                isEnum = r.["typtype"] :?> char = 'e' 
                                                typType= r.["typtype"] :?> char
                                                relName = r.["relname"] :?> string
                                                cname = r.["attname"] :?> string
                                                ctype = r.["tname"] :?> string
                                                cpos = r.["attnum"] :?> int16
                                                cNotNull =r.["attnotnull"] :?> bool
                                                isPK = false // temporarily
                                            } 
                                yield c
                        } |> Seq.groupBy (fun c -> c.schema,c.relName) 
                        |> Seq.map(fun ((schema,relName),cols) -> 
                                                (
                                                    ({schema = schema ; table = relName}:Fss.Data.Common.SchematizedTable),(Array.ofSeq cols))
                                                )
                        |> Map.ofSeq

                // Fetch primary key details
                // Determine which if any columns are a primary key that we could return

                /// Map of table name onto array of column indexes for the primary key columns
                let pkCols = seq {
                                    use comm3 = conn.CreateCommand()
                                    
                                    comm3.CommandText <- "select nspname as namespace,relname,conkey from 
                                                            pg_constraint c 
                                                            JOIN pg_class cl ON c.conrelid = cl.oid 
                                                            join pg_namespace nsp on nsp.oid = cl.relnamespace
                                                            WHERE contype = 'p'  "

                                    use reader3 = comm3.ExecuteReader()
                                    while reader3.Read() do
                                        let table = reader3.["relname"] :?> string
                                        let schema = reader3.["namespace"] :?> string
                                        let conKey = reader3.["conkey"] :?> int16 []
                                        yield ({schema = schema ; table = table}:Fss.Data.Common.SchematizedTable),conKey
                              } 
                                |> Map.ofSeq

                // Take column map (table->coldetails) and flag columns that are primary keys based on
                // the query above.  Map over the table->colDetail map and update colDetails
                cols |> Map.map (fun schemaTable cols ->
                                    match pkCols.TryFind schemaTable with
                                    | None ->cols // no primary key info for this table
                                    | Some(pkColArray) ->
                                         // primary key columns for this table
                                         let colsToChange = pkColArray |> Set.ofSeq
                                         cols |> Array.map (fun c -> 
                                                                if colsToChange.Contains(c.cpos) then 
                                                                    {c with isPK = true} 
                                                                else c
                                                       )
                                )
                
                
    end

    type ISqlConnection = Fss.Data.Common.ISqlConnection
    type DynamicSqlConnection = Fss.Data.Common.DynamicSqlConnection<DuckDBConnection,DuckDBParameter,DuckDBCustomizations>
    type DynamicSqlTransaction = Fss.Data.Common.DynamicSqlTransaction<DuckDBParameter,DuckDBConnection,DuckDBCustomizations>

