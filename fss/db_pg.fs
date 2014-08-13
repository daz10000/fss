﻿namespace Fss.Data

/// Postgres database wrapper.  Independent of other Fss pieces,
/// can be omitted along with System.Data and NPgsql dependencies for a smaller compilation unit
/// or used standalone from other Fss pieces.
/// Credit to Thomas Petricek for the original dynamic operator concept 
open System.IO

module Postgres =
    open System.Data
    open System.Data.Common
    open System.Data.SqlClient
    open System
    open Fss.Pool // used for database handle pooling
    open Npgsql

    type ConnOpts = {  mutable logfile : StreamWriter option ; mutable logfileName : string option ; mutable logQueries : bool;
                            mutable logLongerThan : float ; mutable logConnUse : bool }

    /// SqlDataReader wrapper that provides access to columns 
    /// of the result-set using dynamic access operator
    /// See http://tomasp.net/blog/dynamic-sql.aspx for original idea.
    type DynamicSqlDataReader(reader:NpgsqlDataReader) =
      member x.Reader = reader
      member x.Close() = reader.Close()
      member x.Read() = reader.Read()
      // Read the specified column and casts it to the specified type
      static member (?) (dr:DynamicSqlDataReader, name:string) : 'R = 
        match dr.Reader.[name] with
            | :? DBNull -> unbox null // support for nullable types
            | _ -> unbox (dr.Reader.[name])

      interface IDisposable with
        member x.Dispose() = reader.Dispose()

    /// SqlCommand wrapper that allows setting properties of a
    /// stored procedure using dynamic setter operator
    type DynamicSqlCommand(cmd:NpgsqlCommand,release:unit->unit,opts:ConnOpts,log:string->unit) = 
      let logQuery s =if opts.logQueries then log s
        
      member private x.Command = cmd
      member x.GetConnHash() = cmd.Connection.GetHashCode()
      // Adds parameter with the specified name and value
      static member (?<-) (cmd:DynamicSqlCommand, name:string, value) = 
        let p = NpgsqlParameter("@" + name, box value)
        cmd.Command.Parameters.Add(p) |> ignore
      // Execute command and wrap returned SqlDataReader
      member x.ExecuteReader() =  
                    let start = System.DateTime.Now
                    let r = cmd.ExecuteReader()
                    if opts.logQueries || ((System.DateTime.Now - start).TotalMilliseconds > opts.logLongerThan) then
                        sprintf "%d\t%f\t%s" (x.Command.Connection.GetHashCode()) ((System.DateTime.Now-start).TotalMilliseconds) x.Command.CommandText |> log
                    new DynamicSqlDataReader(r)

      member x.ExecuteNonQuery() = 
                    let start = System.DateTime.Now
                    let r = cmd.ExecuteNonQuery()
                    if opts.logQueries || ((System.DateTime.Now - start).TotalMilliseconds > opts.logLongerThan) then
                        sprintf "%d\t%f\t%s" (x.Command.Connection.GetHashCode()) ((System.DateTime.Now-start).TotalMilliseconds) x.Command.CommandText |> log
                    r
      
      member x.ExecuteScalar() = 
                    let start = System.DateTime.Now
                    let r = cmd.ExecuteScalar()
                    if opts.logQueries || ((System.DateTime.Now - start).TotalMilliseconds > opts.logLongerThan) then
                        sprintf "%d\t%f\t%s" (x.Command.Connection.GetHashCode()) ((System.DateTime.Now-start).TotalMilliseconds) x.Command.CommandText |> log
                    r
      member x.Parameters = cmd.Parameters
      member x.Close() = cmd.Connection.Close()
      interface IDisposable with
        member x.Dispose() = 
            cmd.Dispose()
            release() 

      member x.Transaction with set(v) = cmd.Transaction <- v

    /// Sql Transaction wrapper that encapsulates an active database
    /// connection and hands of DynamicSqlCommand objects with the
    /// connection and transcation set correctly
    type DynamicSqlTransaction(conn:NpgsqlConnection,dispose:unit->unit,opts:ConnOpts,log:string->unit) =
        let trans = conn.BeginTransaction()
        do
            ()
        member x.cc commandText =
            use comm = conn.CreateCommand()
            comm.CommandText <- commandText
            comm.Transaction<-trans
            new DynamicSqlCommand(comm,(fun () -> ()),opts,log)

        member x.Rollback() = trans.Rollback()
        member x.Commit() = trans.Commit()
        member x.InsertMany<'T,'R> (items : 'T seq,?table:string) =
            // Determine table name from item to be inserted
            let table = match table with
                            | Some(x) -> x.ToLower()
                            | None -> typeof<'T>.Name.ToLower()
            conn
        interface IDisposable with
            member x.Dispose() =
                trans.Dispose()
                dispose()

    /// Details of one table column for dynamic record filling
    type ColDetail = { cname : string ; ctype : string ; cpos : int16 ; cNotNull : bool}

    /// SqlConnection wrapper that allows creating stored 
    /// procedure calls using the dynamic access operator
    type DynamicSqlConnection(connStr:string,poolSize:int) =
          let pool = new Pool<NpgsqlConnection>(poolSize,fun _ -> 
                                                let c = new NpgsqlConnection(connStr)
                                                c.Open()
                                                c
       
                                          )

          let opts : ConnOpts =  {logfile = None ; logfileName = None ; logQueries = false;  logLongerThan = 999999.0 ; 
                                    logConnUse = false }
        
          let log (msg:string) =
            match opts.logfile with
                | None -> ()
                | Some(f) -> lock opts.logfile ( fun _ -> f.Write(System.DateTime.Now.ToString("yyyyddMM HH:mm:ss.FFF")); f.Write("\t") ; f.WriteLine(msg) ; f.Flush())
          
          let take() = 
            let c = pool.Take()
            let z = System.Threading.Thread.CurrentThread.ManagedThreadId
            if opts.logConnUse then log(sprintf "take %d %d" z (c.GetHashCode()))
            c
            
          let release x = 
            let z = System.Threading.Thread.CurrentThread.ManagedThreadId
            if opts.logConnUse then log(sprintf "release %d %d" z (x.GetHashCode()))
            sprintf  "release %d" (x.GetHashCode()) |> log; pool.Release x
          
          /// Set a filename for logging output
          member x.Logfile with  get() = opts.logfileName.Value and 
                                set(fileName:string) = 
                                    opts.logfileName <- Some(fileName)
                                    opts.logfile<- Some(new StreamWriter(fileName))

          member x.Take() = take()
          member x.Release(z) = release z
          member x.Opts with get() = opts
          member x.Log(msg:string) = log msg
          member x.LogConnUse with get() = opts.logConnUse and set(v) = opts.logConnUse <- v  
          member x.LogQueries with get() = opts.logQueries and set(v) = opts.logQueries <- v  
          member x.LogLongerThan with get() = opts.logLongerThan  and set(v) = opts.logLongerThan <- v  
                                                    
          member x.InsertMany<'T,'R> (items : 'T seq,?table:string,?transProvided:DynamicSqlTransaction) =
            // Determine table name from item to be inserted
            let table = match table with
                            | Some(x) -> x.ToLower()
                            | None -> typeof<'T>.Name.ToLower()
        
            // Inspect table definition.
            use command : DynamicSqlCommand = x.Command "select a.attname as attname,t.typname as tname,attnum,attnotnull from 
	                                                        pg_class c JOIN pg_attribute a ON c.oid = a.attrelid 
	                                                        JOIN pg_type t ON t.oid = a.atttypid WHERE
	                                                        c.relname = :tablename AND
	                                                        a.attnum > 0"

            command?tablename <- table
            use r = command.ExecuteReader()
            /// db columns
            let cols =
                    seq { while r.Read() do
                            yield { cname = r?attname; ctype = r?tname ; cpos = r?attnum ; cNotNull =r?attnotnull}
                    } |> Array.ofSeq

            if cols.Length = 0 then
                // They probably misnamed the table request
                use comm4 = x.Command "select count(*) from pg_class where relname = :tablename"
                comm4?tablename <- table
                let count = comm4.ExecuteScalar() :?> int64
                if count = 0L then
                    failwithf "ERROR: no such table '%s'" table
                                        
            // Determine which if any columns are a primary key that we could return
            use comm3 = x.Command "select conkey from 
	                        pg_constraint c JOIN pg_class cl ON c.conrelid = cl.oid 
	                        WHERE 
		                        contype = 'p' AND
		                        relname = :tablename
                                "
            comm3?tablename <- table

            // Get columns involved in the primary key if any
            let pKey = comm3.ExecuteScalar() :?> int16 array

            /// db column names
            let dbColNames = cols |> Array.map (fun z -> z.cname ) |> Set.ofSeq
            // Determine which columns were mentioned in the item being inserted.  May
            // be a subset of the available column names, but can't include non columns
            let fields = typeof<'T>.UnderlyingSystemType.GetProperties()
            let colNames = fields |> Array.map (fun z -> z.Name)
            match colNames |> Array.tryFind (fun z-> not  (dbColNames.Contains(z.ToLower()))) with
                    | Some(problem) -> 
                        failwithf "ERROR: record field '%s' does not match a table column in %s [%s]" problem table (String.Join(",",dbColNames))
                    | None -> // clear to proceed

                        let returningClause = match pKey with
                                                | [||] -> "" // no primary key
                                                | [| x |] -> sprintf "returning %s" ((cols |> Array.find (fun z -> z.cpos = x) ).cname)
                                                | _ -> "" // multi column key not supported

                        // Create SQL statement  e.g. something like t his
                        // insert into test (username,host,port,usessl,password,nextuid,uidvalidity,checkcert) values 
                        //                                    (:username,:host,:port,:usessl,:password,:nextuid,
                        //                                       :uidvalidity,:checkcert) returning id"
                        let sql = sprintf "insert into %s(%s) values(%s) %s"  
                                    table 
                                    (String.Join(",",colNames|> Array.map(fun x -> x.ToLower()))) 
                                    (String.Join(",",[| for i in 1..fields.Length -> sprintf ":p%d"i |]))
                                    returningClause
                            
                        /// Transaction to wrap the insertion into    
                        let trans = match transProvided with
                                        | None -> x.StartTrans()  // they didn't give us one, make it
                                        | Some t -> t

                        use comm2 = trans.cc sql
                        // let vals = fields |> Array.mapi (fun i f -> NpgsqlParameter(sprintf "p%d" (i+1),f.GetValue(item,null)))
                        let vals = fields |> Array.mapi (fun i f -> 
                                                            let p = NpgsqlParameter()
                                                            p.ParameterName <- sprintf "p%d" (i+1)
                                                            p
                                                          )

                        comm2.Parameters.AddRange(vals)

                        let ret = seq {
                                        for item in items do
                                            fields |> Array.iteri (fun i f -> vals.[i].Value <- f.GetValue(item,null))
                                            yield (comm2.ExecuteScalar() :?> 'R )
                                    } |> Array.ofSeq

                        if transProvided.IsNone  then
                            // We made our own transaction, commit and dispose
                            trans.Commit()
                            (trans :> IDisposable).Dispose()
                        ret

          member x.InsertOne<'T,'R> (item : 'T,?table:string,?transProvided:DynamicSqlTransaction) = 
            match table,transProvided with
                | None,None -> x.InsertMany<'T,'R>([item]).[0]
                | Some(t),None -> x.InsertMany<'T,'R>([item],table=t).[0]
                | None,Some(t) -> x.InsertMany<'T,'R>([item],transProvided=t).[0]
                | Some(ta),Some(tr) -> x.InsertMany<'T,'R>([item],table=ta,transProvided=tr).[0]

          member x.Query<'T>(sql:string) : seq<'T> =
            //let connection = pool.Take()
            //use command =  connection.CreateCommand()
            //use command = new DynamicSqlCommand(sql,fun () -> pool.Release(connection))
            //command.CommandText <- sql 
            //if x.LogQueries then x.Log(sql)

            use command : DynamicSqlCommand = x.Command sql
            /// Determine details of this record's constructor
            let cons = typeof<'T>.UnderlyingSystemType.GetConstructors() |> Array.filter (fun c -> c.IsConstructor) |> Seq.head
            let argMap = cons.GetParameters() |> Array.mapi (fun i v -> (v.Name.ToLower(),i)) |> Map.ofSeq
            let args = Array.init (cons.GetParameters().Length) (fun _ -> Object())

            seq {
            
                let start = System.DateTime.Now

                use reader = command.ExecuteReader()
                let fieldMap = [for i in 0..reader.Reader.FieldCount-1 -> 
                                    match argMap.TryFind (reader.Reader.GetName(i).ToLower()) with
                                            | Some(x) -> i,x
                                            | None -> failwithf "ERROR: name mapping,  SQL name '%s' not found in target Record" 
                                                            (reader.Reader.GetName(i))
                               ]

                while reader.Read() do            
                        for i,j in fieldMap do
                            args.[j] <- reader.Reader.GetValue(i) 
                        yield cons.Invoke(args) :?> 'T

                if opts.logQueries || ((System.DateTime.Now - start).TotalMilliseconds > opts.logLongerThan) then
                    sprintf "%d\t%f\t%s" (command.GetConnHash()) ((System.DateTime.Now-start).TotalMilliseconds) sql |> x.Log
                    
            } 

          member x.ExecuteScalar(sql:string) =
            if x.LogQueries then x.Log(sql)
            let start = System.DateTime.Now
            use comm = x.Command sql
            let r = comm.ExecuteScalar()

            if opts.logQueries || ((System.DateTime.Now - start).TotalMilliseconds > opts.logLongerThan) then
                    sprintf "%d\t%f\t%s" (x.GetHashCode()) ((System.DateTime.Now-start).TotalMilliseconds) sql |> x.Log
            r


          new(connStr:string) = new DynamicSqlConnection(connStr,10)
          member private x.Pool = pool
          /// Creates command that calls the specified stored procedure
          static member (?) (conn:DynamicSqlConnection, name) = 
            let connection = conn.Take() // Pool.Take()
            use command =  connection.CreateCommand() 
            command.CommandText <- name 
            command.CommandType <- CommandType.StoredProcedure
            new DynamicSqlCommand(command,(fun () -> conn.Release(connection)),conn.Opts,conn.Log)

          member x.cc comm = x.Command comm
          member x.Command(commandText:string) =
            let conn = x.Take()
            use comm = conn.CreateCommand()
            comm.CommandText <- commandText
            //match trans with | None -> () | Some(t) -> comm.Transaction <- t
            new DynamicSqlCommand(comm,(fun () -> x.Release(conn)),x.Opts,x.Log)

          member x.StartTrans() = 
            let conn = x.Take() // reserve a connection that will be shared across the transaction
            new DynamicSqlTransaction(conn,(fun () -> x.Release(conn)),x.Opts,x.Log)

          interface IDisposable with
            member x.Dispose() = 
                // Need to explicitly dispose of the pool as we couldn't instantiate with with the use statement.
                // This involves first casting to an IDisposable class instance)
                (x.Pool :> IDisposable).Dispose()


