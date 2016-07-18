namespace Fss.Data

/// Postgres database wrapper.  Independent of other Fss pieces,
/// can be omitted along with System.Data and NPgsql dependencies for a smaller compilation unit
/// or used standalone from other Fss pieces.
/// Credit to Thomas Petricek for the original dynamic operator concept 
open System.IO
open Npgsql
open System.Data

module Postgres = 

    type PgCustomizations() =  class
        interface Fss.Data.Common.Customization<NpgsqlParameter,NpgsqlConnection> with
//            member x.makeEnum name v =
//                let p = Npgsql.NpgsqlParameter(name,v)
//                p.NpgsqlDbType<-NpgsqlTypes.NpgsqlDbType.Unknown
//                p
            member x.registerEnum<'Enum when 'Enum:(new:unit->'Enum) and 'Enum:struct and 'Enum :> System.ValueType>() =
                NpgsqlConnection.MapEnumGlobally<'Enum>()
                ()

            member x.reloadTypes (conn:NpgsqlConnection) =
                conn.ReloadTypes()
            member x.reopenConnection(conn:NpgsqlConnection) = 
                NpgsqlConnection.ClearPool(conn)
            member x.loadColDetail(conn:NpgsqlConnection) =
               


                /// db columns
                let cols:Map<string,Fss.Data.Common.ColDetail []> =
                        seq { 
                            // Inspect table definition.
                            use command : NpgsqlCommand = conn.CreateCommand()
                            command.CommandText <- "select c.relname,t.typtype,a.attname as attname,t.typname as tname,attnum,attnotnull from 
                                                                pg_class c JOIN pg_attribute a ON c.oid = a.attrelid 
                                                                JOIN pg_type t ON t.oid = a.atttypid WHERE
                                                                -- c.relname = :tablename AND
                                                                a.attnum > 0"
                            use r = command.ExecuteReader()
                            while r.Read() do
                                let c : Fss.Data.Common.ColDetail = 
                                            {   isEnum = r.["typtype"] :?> char = 'e' 
                                                typType= r.["typtype"] :?> char
                                                relName = r.["relname"] :?> string
                                                cname = r.["attname"] :?> string
                                                ctype = r.["tname"] :?> string
                                                cpos = r.["attnum"] :?> int16
                                                cNotNull =r.["attnotnull"] :?> bool
                                                isPK = false // temporarily
                                            } 
                                yield c
                        } |> Seq.groupBy (fun c -> c.relName) 
                        |> Seq.map(fun (relName,cols) -> relName,Array.ofSeq cols)
                        |> Map.ofSeq

                // Fetch primary key details
                // Determine which if any columns are a primary key that we could return

                /// Map of table name onto array of column indexes for the primary key columns
                let pkCols = seq {
                                    use comm3 = conn.CreateCommand()
                                    
                                    comm3.CommandText <- "select relname,conkey from 
                                                    pg_constraint c JOIN pg_class cl ON c.conrelid = cl.oid 
                                                    WHERE contype = 'p'  "

                                    use reader3 = comm3.ExecuteReader()
                                    while reader3.Read() do
                                        let table = reader3.["relname"] :?> string
                                        let conKey = reader3.["conkey"] :?> int16 []
                                        yield table,conKey
                              } 
                                //|> Seq.groupBy (fun (t,c) -> t)
                                //|> Seq.map (fun (t,cols) -> t,cols |> Seq.map (snd) |> Set.ofSeq)
                                |> Map.ofSeq

                // Take column map (table->coldetails) and flag columns that are primary keys based on
                // the query above.  Map over the table->colDetail map and update colDetails
                cols |> Map.map (fun table cols ->
                                    match pkCols.TryFind table with
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
    type DynamicSqlConnection = Fss.Data.Common.DynamicSqlConnection<NpgsqlConnection,NpgsqlParameter,PgCustomizations>
    type DynamicSqlTransaction = Fss.Data.Common.DynamicSqlTransaction<NpgsqlParameter,NpgsqlConnection,PgCustomizations>

