namespace Fss.Data

/// Postgres database wrapper.  Independent of other Fss pieces,
/// can be omitted along with System.Data and NPgsql dependencies for a smaller compilation unit
/// or used standalone from other Fss pieces.
/// Credit to Thomas Petricek for the original dynamic operator concept 
open System.IO

module Common_mysql =
    open System.Data
    open System.Data.Common
    open System.Data.SqlClient
    open System
    open Fss.Pool // used for database handle pooling
    

    type ConnOpts = {  mutable logfile : StreamWriter option ; mutable logfileName : string option ; mutable logQueries : bool;
                            mutable logLongerThan : float ; mutable logConnUse : bool }

    /// Used to detect Option fields that could be null
    let  genericOptionType = typedefof<option<_>>


    /// SqlDataReader wrapper that provides access to columns 
    /// of the result-set using dynamic access operator
    /// See http://tomasp.net/blog/dynamic-sql.aspx for original idea.
    type DynamicSqlDataReader<'Reader> (reader:DbDataReader) =
      member x.Reader = reader
      member x.Close() = reader.Close()
      member x.Read() = reader.Read()
      // Read the specified column and casts it to the specified type
      static member (?) (dr:DynamicSqlDataReader<'Reader>, name:string) : 'R = 
        match dr.Reader.[name] with
            | :? DBNull -> unbox null // support for nullable types
            | _ -> unbox (dr.Reader.[name])

      interface IDisposable with
        member x.Dispose() = reader.Dispose()

    /// SqlCommand wrapper that allows setting properties of a
    /// stored procedure using dynamic setter operator
    type DynamicSqlCommand<'Parameter when 'Parameter :> DbParameter and 'Parameter:(new:unit->'Parameter)>(cmd:DbCommand,release:unit->unit,opts:ConnOpts,log:string->unit) = 
      member private x.Command = cmd
      member x.GetConnHash() = cmd.GetHashCode()
      // Adds parameter with the specified name and value
      static member (?<-) (cmd:DynamicSqlCommand<'Parameter>, name:string, value) = 
        //cmd.Command.Parameters.Add("@"+name,box value) |> ignore
        let p = new 'Parameter() 
        p.ParameterName <- "@"+name
        p.Value <- box value
        cmd.Command.Parameters.Add(p) |> ignore
      // Execute command and wrap returned SqlDataReader
      member x.ExecuteReader() =  
                    let start = System.DateTime.Now
                    let r = cmd.ExecuteReader()
                    if opts.logQueries || ((System.DateTime.Now - start).TotalMilliseconds > opts.logLongerThan) then
                        sprintf "%d\t%f\t%s" (x.Command.Connection.GetHashCode()) ((System.DateTime.Now-start).TotalMilliseconds) x.Command.CommandText |> log
                    new DynamicSqlDataReader<'T>(r)

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
                        sprintf "%d\t%f\t%s" 
                            (x.Command.Connection.GetHashCode()) 
                            ((System.DateTime.Now-start).TotalMilliseconds) 
                            x.Command.CommandText |> log
                    r
      member x.Parameters = cmd.Parameters
      member x.Close() = cmd.Connection.Close()
      interface IDisposable with
        member x.Dispose() = 
            cmd.Dispose()
            release() 

      member x.Transaction with set(v) = cmd.Transaction <- v

   

    /// Details of one table column for dynamic record filling
    // postgresql
    //type ColDetail = { cname : string ; ctype : string ; cpos : int16 ; cNotNull : bool}
    type ColDetail = { cname : string ; ctype : string ; cpos : int16 ; cNotNull : bool}

    /// SqlConnection wrapper that allows creating stored 
    /// procedure calls using the dynamic access operator
    type DynamicSqlConnection<'Conn,'Parameter when 'Parameter :> DbParameter and 'Parameter:(new:unit->'Parameter) and 'Conn :> DbConnection and 'Conn:(new:unit->'Conn) and 'Conn:equality>(connStr:string,poolSize:int) =
          let pool = new Pool<DbConnection>(poolSize,fun _ -> 
                                                let c = new 'Conn()
                                                c.ConnectionString <- connStr
                                                c.Open()
                                                c :> DbConnection
       
                                          )

          let opts : ConnOpts =  {logfile = None ; logfileName = None ; logQueries = false;  logLongerThan = 999999.0 ; 
                                    logConnUse = false }
        
          let log (msg:string) =
            match opts.logfile with
                | None -> ()
                | Some(f) -> lock opts.logfile ( fun _ -> f.Write(System.DateTime.Now.ToString("yyyyddMM HH:mm:ss.FFF")); f.Write("\t") ; f.WriteLine(msg) ; f.Flush())
          
          let takeInternal() = 
            let c = pool.Take()
            let z = System.Threading.Thread.CurrentThread.ManagedThreadId
            if opts.logConnUse then log(sprintf "took thread=%d conn=%d (postfree=%d)" z (c.GetHashCode()) pool.FreeCount) 
            c
            
          let release x = 
            let z = System.Threading.Thread.CurrentThread.ManagedThreadId
            if opts.logConnUse then sprintf "release thread=%d conn=%d (prefree=%d)" z (x.GetHashCode()) pool.FreeCount |> log
            pool.Release x
          
          /// Set a filename for logging output
          member x.Logfile with  get() = opts.logfileName.Value and 
                                set(fileName:string) = 
                                    opts.logfileName <- Some(fileName)
                                    opts.logfile<- Some(new StreamWriter(fileName))

          member x.Take() = takeInternal()
          member x.Release(z) = release z
          member x.Opts with get() = opts
          member x.Log(msg:string) = log msg
          member x.LogConnUse with get() = opts.logConnUse and set(v) = opts.logConnUse <- v  
          member x.LogQueries with get() = opts.logQueries and set(v) = opts.logQueries <- v  
          member x.LogLongerThan with get() = opts.logLongerThan  and set(v) = opts.logLongerThan <- v  
                                                    
          member x.InsertMany<'T,'R> (items : 'T seq,?table:string,?transProvided:DynamicSqlTransaction<'Parameter,'Conn>,?ignoredColumns:string seq) =
            // Determine table name from item to be inserted
            let table = match table with
                            | Some(x) -> x.ToLower()
                            | None -> typeof<'T>.Name.ToLower()
        
            // Determine which columns need to be ignored from the items when inserting into the database.
            let ignoredColumns = 
                match ignoredColumns with
                | Some(cols) -> cols |> Seq.map (fun c -> c.ToLower()) |> Set.ofSeq
                | None -> Set.empty

            // Inspect table definition.
            // flag_LiangMi
            use command : DynamicSqlCommand<'Parameter> = x.Command "SELECT column_name, data_type, ordinal_position, is_nullable FROM 
                                                            information_schema.columns WHERE
                                                            table_name = ?tablename AND ordinal_position > 0"

            command?tablename <- table
            use r = command.ExecuteReader()
            /// db columns
            // flag_LiangMi
            let cols =
                    seq { while r.Read() do
                            //yield { cname = r?column_name; ctype = r?data_type ; cpos = r?ordinal_position ; cNotNull =r?is_nullable}
                            


                            let cpos:int16 = Convert.ToInt16(r?ordinal_position:uint64);
                            let cIsNullable: string = r?is_nullable;
                            yield { 
                            ctype = r?data_type; 
                            cname = r?column_name;
                            cpos = cpos;
                            cNotNull = (match cIsNullable with
                                          | "YES" -> false
                                          | "NO" -> true
                                          | _ as v -> failwithf "Error in cNotNull value = %s" v)
                            }
                    } |> Array.ofSeq

            if cols.Length = 0 then
                // They probably misnamed the table request
                // flag_LiangMi
                use comm4 = x.Command "SELECT count(*) FROM information_schema.tables WHERE table_name = ?tablename"
                comm4?tablename <- table
                let count = comm4.ExecuteScalar() :?> int64
                if count = 0L then
                    failwithf "ERROR: no such table '%s'" table
                                        
            // Determine which if any columns are a primary key that we could return
            // flag_LiangMi
            use comm3 = x.Command "SELECT constraint_name FROM 
                                   information_schema.table_constraints
                                   WHERE 
                                   constraint_schema = 'p' AND
                                   table_name = ?tablename
                                  "
            comm3?tablename <- table

            // Get columns involved in the primary key if any
            let pKey = comm3.ExecuteScalar() :?> int16 array

            /// db column names
            let dbColNames = cols |> Array.map (fun z -> z.cname ) |> Set.ofSeq
            // Determine which columns were mentioned in the item being inserted.  May
            // be a subset of the available column names, but can't include non columns.
            // Filtering out also the columns we do not want to insert in the database, as 
            // specified in ignoredColumns
            let fields = 
                typeof<'T>.UnderlyingSystemType.GetProperties()
                |> Array.filter (fun z -> not (ignoredColumns.Contains(z.Name.ToLower())))
            /// colnames from records
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
                                    (String.Join(",",[| for i in 1..fields.Length  -> sprintf "?p%d"i |]))// flag_LiangMi
                                    returningClause
                            
                        /// Transaction to wrap the insertion into    
                        let trans : DynamicSqlTransaction<'Parameter,'Conn> = 
                                    match transProvided with
                                        | None -> x.StartTrans()  // they didn't give us one, make it
                                        | Some t -> t
                        // flag_LiangMi
                        use comm2 : DynamicSqlCommand<_> = trans.cc sql
                        // let vals = fields |> Array.mapi (fun i f -> NpgsqlParameter(sprintf "p%d" (i+1),f.GetValue(item,null)))

                        (*
                        let vals = [| for i in 1..fields.Length ->
                                            comm2.Parameters.Add(sprintf "p%d" i,null) |]
                        *)
                        
                        let vals = fields |> Array.mapi (fun i f ->  
                                                            let p = new 'Parameter() //:> DbParameter
                                                            p.ParameterName <- sprintf "p%d" (i+1)
                                                            p
                                                          )

                        comm2.Parameters.AddRange(vals)
                        

                        /// Are any of the fields option types / and nullable in database
                        let nullOption = fields |> Array.mapi (fun i f ->
                                                                let pt = f.PropertyType
                                                                let isOption = pt.IsGenericType &&  pt.GetGenericTypeDefinition() = genericOptionType
                                                                let isNotNull = (cols |> Array.find (fun c -> c.cname = f.Name.ToLower())).cNotNull
                                                                match isOption,isNotNull with
                                                                    | true,false -> true
                                                                    | true,true -> failwithf "ERROR: field %s is option but not nullable" f.Name
                                                                    | false,true -> false
                                                                    | false,false -> failwithf "ERROR:field %s is not option but is nullable" f.Name
                                                              )
                        /// Sequence of return values from serially processing each item in items
                        let ret = seq {
                                        for item in items do
                                            //Process the field definitions and poke values into the dbparameter array vals
                                            fields |> Array.iteri (fun i f -> 
                                                                        if nullOption.[i] then
                                                                            // Field can be null and will be defined as an option on the F# record
                                                                            // side so extract differently
                                                                            let v1 = f.GetValue(item,null)
                                                                            vals.[i].Value <- (
                                                                                    if v1=null then null else
                                                                                        let t2 = f.PropertyType.GetProperty("Value")
                                                                                        t2.GetValue(v1,null)
                                                                                        )
                                                                        else
                                                                            vals.[i].Value <- f.GetValue(item,null))
                                            // Finally execute insert stmt and capture return value

                                            yield (comm2.ExecuteScalar() :?> 'R )
                                    } |> Array.ofSeq

                        if transProvided.IsNone  then
                            // We made our own transaction, commit and dispose
                            trans.Commit()
                            (trans :> IDisposable).Dispose()
                        ret

          member x.InsertOne<'T,'R> (item : 'T,?table:string,?transProvided:DynamicSqlTransaction<'Parameter,'Conn>,?ignoredColumns:string seq) = 
            match table,transProvided,ignoredColumns with
            | None,None,None -> x.InsertMany<'T,'R>([item]).[0]
            | Some(t),None,None -> x.InsertMany<'T,'R>([item],table=t).[0]
            | None,Some(t),None -> x.InsertMany<'T,'R>([item],transProvided=t).[0]
            | Some(ta),Some(tr),None -> x.InsertMany<'T,'R>([item],table=ta,transProvided=tr).[0]
            | None,None,Some(cols) -> x.InsertMany<'T,'R>([item],ignoredColumns=cols).[0]
            | Some(t),None,Some(cols) -> x.InsertMany<'T,'R>([item],table=t,ignoredColumns=cols).[0]
            | None,Some(t),Some(cols) -> x.InsertMany<'T,'R>([item],transProvided=t,ignoredColumns=cols).[0]
            | Some(ta),Some(tr),Some(cols) -> x.InsertMany<'T,'R>([item],table=ta,transProvided=tr,ignoredColumns=cols).[0]

          member x.Query<'T>(sql:string,?transProvided:DynamicSqlTransaction<'Parameter,'Conn>) : seq<'T> =
            
            let command : DynamicSqlCommand<'Parameter> = 
                match transProvided with
                | None -> x.Command sql
                | Some(t) -> t.cc sql
                        
            /// Determine details of this record's constructor
            let cons = typeof<'T>.UnderlyingSystemType.GetConstructors() |> Array.filter (fun c -> c.IsConstructor) |> Seq.head

            let isOption = cons.GetParameters() 
                           |> Array.map (fun f -> 
                                            let pt = f.ParameterType 
                                            pt.IsGenericType &&  pt.GetGenericTypeDefinition() = genericOptionType
                                        )
            /// Maps argument name onto positon
            let argMap = cons.GetParameters() |> Array.mapi (fun i v -> (v.Name.ToLower(),i)) |> Map.ofSeq

            /// Array into which we temporarily load values while constructing records
            let args = Array.init (cons.GetParameters().Length) (fun _ -> Object())

            seq {
            
                let start = System.DateTime.Now
                try
                    use reader = command.ExecuteReader()
                    let fieldMap = [
                        for i in 0..reader.Reader.FieldCount-1 -> 
                            match argMap.TryFind (reader.Reader.GetName(i).ToLower()) with
                            | Some(x) -> i,x
                            | None -> failwithf "ERROR: name mapping,  SQL name '%s' not found in target Record" 
                                            (reader.Reader.GetName(i))
                    ]

                    while reader.Read() do            
                        for i,j in fieldMap do
                            args.[j] <- (
                                if isOption.[j] then
                                    match reader.Reader.GetValue(i) with
                                    | :? DBNull ->box None
                                    | :? string as x -> box (Some(x))
                                    | :? int64 as x -> box (Some(x))
                                    | :? float as x -> box (Some(x))
                                    | :? single as x -> box (Some(x))// flag_LiangMi
                                    | :? bool as x -> box (Some(x))
                                    | :? int32 as x -> box (Some(x))
                                    | :? DateTime as x -> box (Some(x))
                                    | _ as x -> failwithf "ERROR: unsupported nullable dbtype %s" (x.GetType().Name)
                                else
                                    reader.Reader.GetValue(i) 
                                )
                        yield cons.Invoke(args) :?> 'T
                finally
                    // Don't dispose till the sequence is finally used. (within sequence generator)
                    (command :> IDisposable).Dispose()
            } 

          member x.ExecuteScalar(sql:string) =
            use comm = x.Command sql
            comm.ExecuteScalar()

          new(connStr:string) = new DynamicSqlConnection<'Conn,'Parameter>(connStr,10)
          member private x.Pool = pool
          /// Creates command that calls the specified stored procedure
          static member (?) (conn:DynamicSqlConnection<'Conn,'Parameter>, name) = 
            let connection = conn.Take() 
            use command =  connection.CreateCommand() 
            command.CommandText <- name 
            command.CommandType <- CommandType.StoredProcedure
            new DynamicSqlCommand<'Parameter>(command,(fun () -> conn.Release(connection)),conn.Opts,conn.Log)

          member x.cc comm = x.Command comm
          member x.Command(commandText:string) =
            let conn = x.Take()
            let comm = conn.CreateCommand()
            comm.CommandText <- commandText
            //match trans with | None -> () | Some(t) -> comm.Transaction <- t
            new DynamicSqlCommand<'Parameter>(comm,(fun () -> x.Release(conn)),x.Opts,x.Log)

          member x.StartTrans() = 
            //let conn = x.Take() // reserve a connection that will be shared across the transaction
            //new DynamicSqlTransaction<'Parameter>(conn,(fun () -> x.Release(conn)),x.Opts,x.Log)
            new DynamicSqlTransaction<'Parameter,'Conn>(x,x.Opts,x.Log)

          interface IDisposable with
            member x.Dispose() = 
                // Need to explicitly dispose of the pool as we couldn't instantiate with with the use statement.
                // This involves first casting to an IDisposable class instance)
                (x.Pool :> IDisposable).Dispose()


    /// Sql Transaction wrapper that encapsulates an active database
    /// connection and hands of DynamicSqlCommand objects with the
    /// connection and transcation set correctly
    //and DynamicSqlTransaction<'Parameter when 'Parameter :> DbParameter and 'Parameter:(new:unit->'Parameter)>(conn:DbConnection,dispose:unit->unit,opts:ConnOpts,log:string->unit) =
    and DynamicSqlTransaction<'Parameter,
                              'Conn when 'Parameter :> DbParameter 
                                    and 'Parameter:(new:unit->'Parameter)  
                                    and 'Conn :> DbConnection 
                                    and 'Conn:(new:unit->'Conn) 
                                    and 'Conn:equality>(conn:DynamicSqlConnection<'Conn,'Parameter>,
                                                        opts:ConnOpts,
                                                        log:string->unit) =
        let baseConn :DbConnection = conn.Take()
        let trans = baseConn.BeginTransaction()
        do
            ()
        member x.cc commandText =
            let comm = baseConn.CreateCommand()
            comm.CommandText <- commandText
            comm.Transaction<-trans
            new DynamicSqlCommand<'Parameter>(comm,(fun () -> comm.Dispose()),opts,log)

        member x.Rollback() = trans.Rollback()

        member x.Commit() = trans.Commit()

        member x.Query<'T>(sql:string) : seq<'T> =
            conn.Query(sql,transProvided=x)

        member x.InsertMany<'T,'R> (items : 'T seq,?table:string,?ignoredColumns: string seq) =
            match table,ignoredColumns with
            | None,None -> conn.InsertMany<'T,'R>(items,transProvided=x)
            | Some(ta),None -> conn.InsertMany<'T,'R>(items,table=ta,transProvided=x)
            | None,Some(cols) -> conn.InsertMany<'T,'R>(items,ignoredColumns=cols,transProvided=x)
            | Some(ta),Some(cols) -> conn.InsertMany<'T,'R>(items,table=ta,ignoredColumns=cols,transProvided=x)

        member x.InsertOne<'T,'R> (item : 'T,?table:string,?ignoredColumns:string seq) = 
            match table,ignoredColumns with
            | None,None -> conn.InsertMany<'T,'R>([item],transProvided=x).[0]
            | Some(ta),None -> conn.InsertMany<'T,'R>([item],table=ta,transProvided=x).[0]
            | None,Some(cols) -> conn.InsertMany<'T,'R>([item],ignoredColumns=cols,transProvided=x).[0]
            | Some(ta),Some(cols) -> conn.InsertMany<'T,'R>([item],table=ta,ignoredColumns=cols,transProvided=x).[0]

        member x.ExecuteScalar(sql:string) =
            use comm = baseConn.CreateCommand()
            comm.CommandText <- sql
            comm.Transaction<-trans
            comm.ExecuteScalar()

        interface IDisposable with
            member x.Dispose() =
                trans.Dispose()
                conn.Release baseConn
