namespace Fss.Data
open System.IO

/// Database-agnostic core shared by fss_duckdb, fss_postgres and fss_sqlite.
/// Each of those packages compiles this file in and supplies a `Customization` that
/// handles the dialect-specific parts (metadata queries, parameter syntax, key retrieval).
/// Credit to Thomas Petricek for the original dynamic operator concept
/// (http://tomasp.net/blog/dynamic-sql.aspx).

module Common =
    open System
    open System.Data
    open System.Data.Common
    open System.Diagnostics
    open System.Globalization
    open System.Reflection
    open Microsoft.FSharp.Reflection
    open Fss.Pool // used for database handle pooling

    type SchematizedTable = { schema : string ; table : string}

    /// Details of one table column for dynamic record filling
    type ColDetail = {  schema : string
                        typType : char
                        isEnum : bool
                        cname : string
                        ctype : string
                        cpos : int16
                        cNotNull : bool
                        relName : string
                        isPK    : bool
                     }

    /// How a dialect returns the key of a freshly inserted row
    type SequenceMechanism =
            /// `insert ... returning <pk>` (Postgres, DuckDB)
            | RETURNS_CLAUSE
            /// run this SQL after the insert, on the same connection (e.g. SQLite `select last_insert_rowid()`)
            | SQL_STMT of string

    /// Dialect-specific behaviour. Implemented once per database package.
    type Customization<'Parameter,'Conn when 'Parameter :> DbParameter
                                    and 'Parameter:(new:unit->'Parameter)
                                    and 'Conn :> DbConnection
                                    > =
        /// Refresh any driver-side type information (e.g. Postgres enums) on one connection
        abstract member reloadTypes:'Conn->unit
        /// Load column metadata for every visible table
        abstract member loadColDetail:'Conn->Map<SchematizedTable,ColDetail []>
        /// Try to bring a broken connection back (used by the keep-alive thread)
        abstract member reopenConnection:'Conn->unit
        abstract member sequenceMechanism:unit->SequenceMechanism
        /// Whether to run a background thread that pings the server and reopens dropped connections
        abstract member needsKeepAlive:unit->bool
        /// Schemas to search, in order, when resolving an unqualified table name for inserts
        abstract member getSearchPath:'Conn->string list
        /// Create and open a connection for the pool. `first` is the first connection already
        /// created for this pool (None when creating the first). Lets in-process engines such as
        /// DuckDB share one database between pooled connections.
        abstract member openConnection:connStr:string * first:'Conn option -> 'Conn
        /// Prefix for named parameters in generated SQL, e.g. "@" (Postgres/SQLite) or "$" (DuckDB)
        abstract member parameterPrefix:unit->string

    type ConnOpts = {  mutable logfile : StreamWriter option ; mutable logfileName : string option ; mutable logQueries : bool;
                            mutable logLongerThan : float ; mutable logConnUse : bool }

    /// Used to detect Option fields that could be null
    let  genericOptionType = typedefof<option<_>>

    // ---------------------------------------------------------------------------------------
    // Value conversion between what the ADO.NET driver returns and what the F# type expects
    // ---------------------------------------------------------------------------------------

    let internal isOptionType (t:Type) = t.IsGenericType && t.GetGenericTypeDefinition() = genericOptionType

    let internal isDbNull (v:obj) = isNull v || v :? DBNull

    /// Convert a non-null database value to `target` when the driver returned a different
    /// (but compatible) type, e.g. int64 -> int, int64 -> bool (SQLite), BigInteger -> int64 (DuckDB HUGEINT).
    let internal coerce (target:Type) (v:obj) : obj =
        if target.IsInstanceOfType v then v
        elif target.IsEnum then
            match v with
            | :? string as s -> Enum.Parse(target, s, true)
            | _ -> Enum.ToObject(target, Convert.ChangeType(v, Enum.GetUnderlyingType target, CultureInfo.InvariantCulture))
        else
            match v with
            | :? DateTime as d when target = typeof<DateOnly> -> box (DateOnly.FromDateTime d)
            | :? DateOnly as d when target = typeof<DateTime> -> box (d.ToDateTime TimeOnly.MinValue)
            | :? TimeSpan as t when target = typeof<TimeOnly> -> box (TimeOnly.FromTimeSpan t)
            | :? TimeOnly as t when target = typeof<TimeSpan> -> box (t.ToTimeSpan())
            | :? DateTime as d when target = typeof<DateTimeOffset> -> box (DateTimeOffset d)
            | :? string as s when target = typeof<Guid> -> box (Guid.Parse s)
            | :? Stream as st when target = typeof<byte[]> ->
                // e.g. DuckDB returns BLOB columns as a stream
                use ms = new MemoryStream()
                st.CopyTo ms
                box (ms.ToArray())
            | :? Numerics.BigInteger as b ->
                // HUGEINT and friends: go via decimal (covers +/- 7.9e28), which is IConvertible
                Convert.ChangeType(decimal b, target, CultureInfo.InvariantCulture)
            | :? IConvertible when typeof<IConvertible>.IsAssignableFrom target ->
                Convert.ChangeType(v, target, CultureInfo.InvariantCulture)
            | _ -> v // let the caller report the mismatch

    /// Build a function mapping a raw reader value (possibly DBNull) onto a value of type `target`.
    /// Option and Nullable targets map NULL to None/null; other value types reject NULL with a clear error.
    let internal valueConverter (fieldName:string) (target:Type) : obj -> obj =
        if isOptionType target then
            let inner = target.GetGenericArguments().[0]
            let someCase = FSharpType.GetUnionCases(target) |> Array.find (fun c -> c.Name = "Some")
            let mkSome = FSharpValue.PreComputeUnionConstructor someCase
            fun v -> if isDbNull v then null (* None *) else mkSome [| coerce inner v |]
        else
            match Nullable.GetUnderlyingType target with
            | null when target.IsValueType ->
                fun v ->
                    if isDbNull v then
                        failwithf "ERROR: column '%s' is NULL but field type %s is not an option (or Nullable)" fieldName target.Name
                    else coerce target v
            | null -> fun v -> if isDbNull v then null else coerce target v
            | underlying -> fun v -> if isDbNull v then null else coerce underlying v

    /// Strip a leading parameter marker so callers can write "id", "@id", ":id" or "$id"
    let internal bareParamName (name:string) =
        if name.Length > 1 && (name.[0] = '@' || name.[0] = ':' || name.[0] = '$') then name.Substring 1 else name

    /// Convert an F# value into something the ADO.NET driver accepts as a parameter value
    let internal toDbValue (value:obj) : obj =
        if isNull value then box DBNull.Value
        else
            let t = value.GetType()
            if isOptionType t then
                // Some x -> x  (None is represented as null and handled above)
                t.GetProperty("Value").GetValue(value)
            else value

    /// SqlDataReader wrapper that provides access to columns
    /// of the result-set using dynamic access operator
    /// See http://tomasp.net/blog/dynamic-sql.aspx for original idea.
    type DynamicSqlDataReader<'Reader> (reader:DbDataReader) =
      member x.Reader = reader
      member x.Close() = reader.Close()
      member x.Read() = reader.Read()
      /// Read the named column and convert it to the requested type, e.g. `let age : int option = r?age`
      static member (?) (dr:DynamicSqlDataReader<'Reader>, name:string) : 'R =
        let v = dr.Reader.[name]
        valueConverter name typeof<'R> v |> unbox<'R>

      interface IDisposable with
        member x.Dispose() = reader.Dispose()

    /// DbCommand wrapper that allows setting parameters
    /// using the dynamic setter operator: `cmd?name <- value`
    type DynamicSqlCommand<'Parameter when 'Parameter :> DbParameter and 'Parameter:(new:unit->'Parameter)>(cmd:DbCommand,release:unit->unit,opts:ConnOpts,log:string->unit) =
      let timed (f:unit -> 'a) =
            let sw = Stopwatch.StartNew()
            let r = f()
            let ms = sw.Elapsed.TotalMilliseconds
            if opts.logQueries || ms > opts.logLongerThan then
                sprintf "%d\t%f\t%s" (cmd.Connection.GetHashCode()) ms cmd.CommandText |> log
            r
      let mutable disposed = false

      member private x.Command = cmd
      member x.GetConnHash() = cmd.GetHashCode()

      /// Add a named parameter. The name may be given with or without its marker ("id", "@id", "$id").
      member x.AddParameter(name:string, value:obj) =
        let p = new 'Parameter()
        p.ParameterName <- bareParamName name
        p.Value <- toDbValue value
        cmd.Parameters.Add(p) |> ignore

      /// Adds parameter with the specified name and value
      static member (?<-) (cmd:DynamicSqlCommand<'Parameter>, name:string, value) =
        cmd.AddParameter(name, box value)

      /// Execute command and wrap returned DbDataReader
      member x.ExecuteReader() = new DynamicSqlDataReader<'T>(timed cmd.ExecuteReader)
      member x.ExecuteNonQuery() = timed cmd.ExecuteNonQuery
      member x.ExecuteScalar() = timed cmd.ExecuteScalar
      member x.Parameters = cmd.Parameters
      member x.Close() = cmd.Connection.Close()
      interface IDisposable with
        member x.Dispose() =
            if not disposed then
                disposed <- true
                cmd.Dispose()
                release()

      member x.Transaction with set(v) = cmd.Transaction <- v

    let internal addParameters (cmd:DynamicSqlCommand<'P>) (parameters:seq<string*obj> option) =
        match parameters with
        | None -> ()
        | Some ps -> for (name,value) in ps do cmd.AddParameter(name, value)

    /// Describes how to build one result row of type 'T from a DbDataReader
    type internal RowBuilder = { build : DbDataReader -> obj }

    /// Work out, once per query, how to turn reader rows into values of type t:
    ///  - F# tuples: columns by position
    ///  - records / classes with a constructor: columns by (case-insensitive) name
    ///  - anything else (int, decimal, DateTime, Guid, string, option, ...): the first column
    let internal makeRowBuilder (t:Type) (reader:DbDataReader) : RowBuilder =
        let colName i = reader.GetName(i)
        /// Map columns by name only for records and classes. Structs such as decimal, DateTime and
        /// Guid have parameterised constructors too, but are single values (issue #1).
        let mapsByName =
            FSharpType.IsRecord(t, BindingFlags.Public ||| BindingFlags.NonPublic)
            || (t.IsClass && t <> typeof<string> && not t.IsArray && not (isOptionType t))
        if FSharpType.IsTuple t then
            let elems = FSharpType.GetTupleElements t
            if reader.FieldCount <> elems.Length then
                failwithf "ERROR: query returned %d columns but target tuple has %d elements" reader.FieldCount elems.Length
            let convs = elems |> Array.mapi (fun i et -> valueConverter (colName i) et)
            let ctor = FSharpValue.PreComputeTupleConstructor t
            { build = fun r -> ctor (Array.init elems.Length (fun i -> convs.[i] (r.GetValue i))) }
        else
            let cons =
                t.GetConstructors(BindingFlags.Public ||| BindingFlags.Instance)
                |> Array.sortByDescending (fun c -> c.GetParameters().Length)
                |> Array.tryHead
            match cons with
            | Some cons when mapsByName && cons.GetParameters().Length > 0 ->
                let ps = cons.GetParameters()
                /// Maps argument name onto position
                let argMap = ps |> Array.mapi (fun i p -> (p.Name.ToLower(),i)) |> Map.ofSeq
                let fieldMap =
                    [| for i in 0..reader.FieldCount-1 ->
                        match argMap.TryFind (colName(i).ToLower()) with
                        | Some j -> i,j
                        | None -> failwithf "ERROR: name mapping,  SQL name '%s' not found in target Record %s" (colName i) t.Name |]
                let supplied = fieldMap |> Array.map snd |> Set.ofArray
                // Option fields the query doesn't return become None; anything else missing is an error
                let missing =
                    ps |> Array.indexed
                    |> Array.filter (fun (j,p) -> not (supplied.Contains j) && not (isOptionType p.ParameterType))
                if missing.Length > 0 then
                    failwithf "ERROR: record %s field(s) %s not returned by the query" t.Name
                        (String.Join(",", missing |> Array.map (fun (_,p) -> p.Name)))
                let convs = ps |> Array.map (fun p -> valueConverter p.Name p.ParameterType)
                { build = fun r ->
                    let args : obj[] = Array.zeroCreate ps.Length
                    for (i,j) in fieldMap do
                        let raw = r.GetValue i
                        args.[j] <-
                            try convs.[j] raw
                            with e -> failwithf "ERROR: cannot map column '%s' (%s) onto field %s : %s - %s"
                                            (colName i) (if isDbNull raw then "NULL" else raw.GetType().Name)
                                            ps.[j].Name ps.[j].ParameterType.Name e.Message
                    try cons.Invoke(args)
                    with :? ArgumentException as e ->
                        failwithf "ERROR: could not construct %s from row (%s). Check the field types match the column types." t.Name e.Message }
            | _ ->
                let conv = valueConverter (if reader.FieldCount > 0 then colName 0 else "?") t
                { build = fun r -> conv (r.GetValue 0) }

    /// Database operations available on both a connection and a transaction, so code can be
    /// written once and run either inside or outside a transaction.
    type ISqlConnection =
        /// Run a query and map each row onto 'T (record by column name, tuple by position, or scalar).
        abstract member Query : sql:string * ?parameters:seq<string*obj> -> seq<'T>
        abstract member InsertMany : items:seq<'T> * ?table:string * ?ignoredColumns:seq<string> -> 'R []
        abstract member InsertOne : item:'T * ?table:string * ?ignoredColumns:seq<string> -> 'R
        abstract member ExecuteScalar : sql:string * ?parameters:seq<string*obj> -> obj
        abstract member ExecuteNonQuery : sql:string * ?parameters:seq<string*obj> -> int

    /// Pooled database connection that maps query results onto F# records and inserts F# records.
    and DynamicSqlConnection<'Conn,'Parameter,'Customizer when 'Parameter :> DbParameter
                                and 'Parameter:(new:unit->'Parameter)
                                and 'Conn :> DbConnection
                                and 'Conn:(new:unit->'Conn)
                                and 'Conn:equality
                                and 'Customizer :> Customization<'Parameter,'Conn>
                                and 'Customizer:(new:unit -> 'Customizer)
                                >(connStr:string,poolSize:int)  =
          /// This class provides any functions we will need to handle
          /// db flavor specific customization
          let customizer = new 'Customizer()

          /// Monitor and restore connection?
          let mutable keepAlive = customizer.needsKeepAlive()
          /// flag when connection is disposed to ensure keep alive shuts down gracefully
          let mutable hasDisposed = false

          /// lockable object to ensure keep alive and dispose don't fight
          let disposeLock = obj()

          /// Interval between tests that connections are alive
          let mutable keepAliveInterval = 5000

          let mutable firstConn : 'Conn option = None
          let pool = new Pool<DbConnection>(poolSize,fun _ ->
                                                let c = customizer.openConnection(connStr, firstConn)
                                                if firstConn.IsNone then firstConn <- Some c
                                                c :> DbConnection
                                          )

          let opts : ConnOpts =  {logfile = None ; logfileName = None ; logQueries = false;  logLongerThan = 999999.0 ;
                                    logConnUse = false }

          let withPooledConn (f:'Conn -> 'a) =
            let conn = pool.Take()
            try f (conn :?> 'Conn)
            finally pool.Release conn

          let fetchColData() = withPooledConn customizer.loadColDetail

          /// Table specific meta data, and schema search path. If either fails, don't leak the pool.
          let mutable colMetadata, searchPath =
            try
                fetchColData(), withPooledConn customizer.getSearchPath
            with _ ->
                (pool :> IDisposable).Dispose()
                reraise()

          let log (msg:string) =
            match opts.logfile with
                | None -> ()
                | Some(f) -> lock f ( fun _ -> f.Write(System.DateTime.Now.ToString("yyyyMMdd HH:mm:ss.FFF")); f.Write("\t") ; f.WriteLine(msg) ; f.Flush())

          let takeInternal() =
            let c = pool.Take()
            let z = System.Threading.Thread.CurrentThread.ManagedThreadId
            if opts.logConnUse then log(sprintf "took thread=%d conn=%d (postfree=%d)" z (c.GetHashCode()) pool.FreeCount)
            c

          let release x =
            let z = System.Threading.Thread.CurrentThread.ManagedThreadId
            if opts.logConnUse then sprintf "release thread=%d conn=%d (prefree=%d)" z (x.GetHashCode()) pool.FreeCount |> log
            pool.Release x

          let keepAliveThread() =
            let mutable wasOpen = true
            while not hasDisposed do
                lock disposeLock (
                        fun () ->
                            if not hasDisposed then
                                let conn = takeInternal()
                                try
                                    use comm = conn.CreateCommand()
                                    comm.CommandText <- "select 1 as x"
                                    comm.ExecuteScalar() |> ignore
                                    release conn
                                    if not wasOpen then
                                        log "Keep alive: connection restored"
                                        wasOpen <- true
                                with x ->
                                    release conn
                                    wasOpen <- false
                                    log (sprintf "Keep alive: ping failed (%s), reopening connection pool" x.Message)
                                    pool.Iter(fun conn -> customizer.reopenConnection (conn:?>'Conn) )
                        )
                System.Threading.Thread.Sleep(keepAliveInterval)

          do
            if keepAlive then
                // Background thread so a forgotten Dispose doesn't stop the process from exiting
                let t = new System.Threading.Thread(keepAliveThread, IsBackground = true, Name = "fss keep-alive")
                t.Start()

          /// Look up the column metadata for a table, refreshing the cache once if it isn't known
          /// (e.g. the table was created after this connection was opened). `table` is either a bare
          /// name, resolved through the schema search path, or schema-qualified ("schema.table").
          let findTable (table:string) =
            let lookup () =
                match table.Split('.') with
                | [| schema ; name |] -> colMetadata.TryFind {schema = schema ; table = name}
                | _ -> searchPath |> List.tryPick (fun schema -> colMetadata.TryFind {schema = schema ; table = table})
            match lookup () with
            | Some _ as found -> found
            | None ->
                colMetadata <- fetchColData()
                lookup ()

          /// Set a filename for logging output
          member x.Logfile with  get() = defaultArg opts.logfileName null and
                                set(fileName:string) =
                                    opts.logfile |> Option.iter (fun f -> f.Dispose())
                                    opts.logfileName <- Some(fileName)
                                    opts.logfile<- Some(new StreamWriter(fileName))
          member x.Take() = takeInternal()
          member x.Release(z) = release z
          member x.KeepAlive with get() = keepAlive and set(v) = keepAlive<-v
          member x.Opts with get() = opts
          member x.Log(msg:string) = log msg
          member x.LogConnUse with get() = opts.logConnUse and set(v) = opts.logConnUse <- v
          member x.LogQueries with get() = opts.logQueries and set(v) = opts.logQueries <- v
          member x.LogLongerThan with get() = opts.logLongerThan  and set(v) = opts.logLongerThan <- v

          /// Reload driver type information (e.g. after creating a Postgres enum) and table metadata
          member x.Reload() =
            // For each connection in the pool, run the customization provided function
            // over it to reload any type information.
            pool.Iter(fun conn -> customizer.reloadTypes (conn:?>'Conn) )
            colMetadata <- fetchColData()

          interface ISqlConnection with
              member x.Query<'T>(sql, parameters) : seq<'T> =
                  x.Query<'T>(sql, ?parameters=parameters)
              member x.InsertMany<'T,'R>(items, table, ignoredColumns) =
                  x.InsertMany<'T,'R>(items, ?table=table, ?ignoredColumns=ignoredColumns)
              member x.InsertOne<'T,'R>(item, table, ignoredColumns) =
                  x.InsertOne<'T,'R>(item, ?table=table, ?ignoredColumns=ignoredColumns)
              member x.ExecuteScalar(sql, parameters) =
                  x.ExecuteScalar(sql, ?parameters=parameters)
              member x.ExecuteNonQuery(sql, parameters) =
                  x.ExecuteNonQuery(sql, ?parameters=parameters)

          /// Insert records into `table` (default: the record type name, lower-cased; may be
          /// schema-qualified, e.g. "sales.orders") and return the
          /// generated primary key of each row as 'R. Record fields must match table columns
          /// (case-insensitive); list database-generated columns (serial ids, defaults) in `ignoredColumns`.
          member x.InsertMany<'T,'R> (items : 'T seq,?table:string,?transProvided:DynamicSqlTransaction<'Parameter,'Conn,'Customizer>,?ignoredColumns:string seq) =
            // Determine table name from item to be inserted
            let table = match table with
                            | Some(x) -> x.ToLower()
                            | None -> typeof<'T>.Name.ToLower()

            // Determine which columns need to be ignored from the items when inserting into the database.
            let ignoredColumns =
                match ignoredColumns with
                | Some(cols) -> cols |> Seq.map (fun c -> c.ToLower()) |> Set.ofSeq
                | None -> Set.empty

            match findTable table with
                | None ->
                      // They probably misnamed the table request
                      failwithf "ERROR: no such table '%s' (searched schemas: %s)" table (String.Join(",", searchPath))
                | Some(cols) ->

                    /// db column names
                    let dbColNames = cols |> Array.map (fun z -> z.cname.ToLower()) |> Set.ofSeq
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
                                let pkCols = cols |> Array.choose (fun c -> if c.isPK then Some c.cname else None)

                                let seqMechanism = customizer.sequenceMechanism()
                                let returningClause = match seqMechanism with
                                                        | RETURNS_CLAUSE ->
                                                            match pkCols with
                                                                | [||] -> "" // no primary key
                                                                | [| x |] -> sprintf "returning %s" x
                                                                | _ -> "" // multi column key not supported
                                                        | _ -> "" // nothing needed here if we don't use the returns pattern
                                // all cols come from the one table, so take its schema and name from the first
                                let schema = cols.[0].schema
                                let tableName = cols.[0].relName
                                let prefix = customizer.parameterPrefix()
                                // Create SQL statement  e.g. something like this
                                // insert into test (username,host,port) values (@p1,@p2,@p3) returning id
                                let sql = sprintf "insert into %s.%s(%s) values(%s) %s"
                                            schema
                                            tableName
                                            (String.Join(",",colNames|> Array.map(fun x -> x.ToLower())))
                                            (String.Join(",",[| for i in 1..fields.Length  -> sprintf "%sp%d" prefix i |]))
                                            returningClause

                                /// Are any of the fields option types / and nullable in database
                                let nullOption =
                                    fields
                                    |> Array.map (fun f ->
                                                    let pt = f.PropertyType
                                                    let isOption = isOptionType pt
                                                    let isNotNull = (cols |> Array.find (fun c -> c.cname.ToLower() = f.Name.ToLower())).cNotNull
                                                    match isOption,isNotNull with
                                                        | true,false -> true
                                                        | true,true -> failwithf "ERROR: field %s is option but not nullable in db" f.Name
                                                        | false,true -> false
                                                        | false,false -> failwithf "ERROR:field %s is not option but is nullable in db" f.Name
                                                    )

                                /// Key values come back as whatever the driver uses (e.g. int64); convert to 'R
                                let toResult (v:obj) : 'R =
                                    if isDbNull v then Unchecked.defaultof<'R>
                                    else coerce typeof<'R> v |> unbox<'R>

                                /// Transaction to wrap the insertion into
                                let trans : DynamicSqlTransaction<'Parameter,'Conn,'Customizer> =
                                            match transProvided with
                                                | None -> x.StartTrans()  // they didn't give us one, make it
                                                | Some t -> t
                                try
                                    use comm2 : DynamicSqlCommand<_> = trans.cc sql

                                    /// Named parameters matching each required field
                                    let vals = fields |> Array.mapi (fun i _ ->
                                                                        let p = new 'Parameter()
                                                                        p.ParameterName <- sprintf "p%d" (i+1)
                                                                        p
                                                                        )

                                    comm2.Parameters.AddRange(vals)

                                    /// Return values from serially processing each item in items
                                    let ret =
                                        [|
                                            for item in items do
                                                //Process the field definitions and poke values into the dbparameter array vals
                                                fields |>
                                                    Array.iteri
                                                        (fun i f ->
                                                            let v = f.GetValue(item,null)
                                                            vals.[i].Value <- if nullOption.[i] then toDbValue v else v)
                                                match seqMechanism with
                                                    | RETURNS_CLAUSE ->
                                                        // Finally execute insert stmt and capture return value
                                                        yield toResult (comm2.ExecuteScalar())
                                                    | SQL_STMT(seqRetrievalSQL) ->
                                                        comm2.ExecuteNonQuery() |> ignore
                                                        use comm3 : DynamicSqlCommand<_> = trans.cc seqRetrievalSQL
                                                        yield toResult (comm3.ExecuteScalar())
                                        |]

                                    if transProvided.IsNone then trans.Commit()
                                    ret
                                finally
                                    // We made our own transaction: dispose it (which rolls back if we didn't commit)
                                    if transProvided.IsNone then (trans :> IDisposable).Dispose()

          /// Insert one record; see InsertMany
          member x.InsertOne<'T,'R> (
                                    item : 'T,
                                    ?table:string,
                                    ?transProvided:DynamicSqlTransaction<'Parameter,'Conn,'Customizer>,
                                    ?ignoredColumns:string seq) =
            x.InsertMany<'T,'R>([item], ?table=table, ?transProvided=transProvided, ?ignoredColumns=ignoredColumns).[0]

          /// Run a query and map each row onto 'T: a record (columns matched to fields by name,
          /// case-insensitive), a tuple (columns by position) or a single value (first column).
          /// The query runs, and a pooled connection is held, only while the sequence is being
          /// enumerated; materialise it (Array.ofSeq / List.ofSeq) if you need it more than once.
          member x.Query<'T>(sql:string,?parameters:seq<string*obj>,?transProvided:DynamicSqlTransaction<'Parameter,'Conn,'Customizer>) : seq<'T> =
            seq {
                use command : DynamicSqlCommand<'Parameter> =
                    match transProvided with
                    | None -> x.Command sql
                    | Some(t) -> t.cc sql
                addParameters command parameters
                use reader = command.ExecuteReader()
                let builder = makeRowBuilder typeof<'T> reader.Reader
                while reader.Read() do
                    yield builder.build reader.Reader :?> 'T
            }

          /// Run a SQL statement and return the first column of the first row
          member x.ExecuteScalar(sql:string, ?parameters:seq<string*obj>) =
            use comm = x.Command sql
            addParameters comm parameters
            comm.ExecuteScalar()

          /// Run a SQL statement (DDL, update, delete, ...) and return the number of rows affected
          member x.ExecuteNonQuery(sql:string, ?parameters:seq<string*obj>) =
            use comm = x.Command sql
            addParameters comm parameters
            comm.ExecuteNonQuery()

          new(connStr:string) = new DynamicSqlConnection<'Conn,'Parameter,'Customizer>(connStr,10)
          member private x.Pool = pool
          /// Creates command that calls the specified stored procedure
          static member (?) (conn:DynamicSqlConnection<'Conn,'Parameter,'Customizer>, name) =
            let connection = conn.Take()
            let command =  connection.CreateCommand()
            command.CommandText <- name
            command.CommandType <- CommandType.StoredProcedure
            new DynamicSqlCommand<'Parameter>(command,(fun () -> conn.Release(connection)),conn.Opts,conn.Log)

          member x.cc comm = x.Command comm
          /// Create a command on a pooled connection. Dispose it (`use`) to return the connection to the pool.
          member x.Command(commandText:string) =
            let conn = x.Take()
            let comm = conn.CreateCommand()
            comm.CommandText <- commandText
            new DynamicSqlCommand<'Parameter>(comm,(fun () -> x.Release(conn)),x.Opts,x.Log)

          /// Start a transaction. It holds one pooled connection until disposed; uncommitted work is rolled back on Dispose.
          member x.StartTrans() =
            new DynamicSqlTransaction<'Parameter,'Conn,'Customizer>(x,x.Opts,x.Log)

          interface IDisposable with
            member x.Dispose() =
                // Need to explicitly dispose of the pool as we couldn't instantiate with with the use statement.
                // This involves first casting to an IDisposable class instance)
                lock disposeLock (fun () ->
                                    if not hasDisposed then
                                        hasDisposed<-true
                                        (x.Pool :> IDisposable).Dispose()
                                        opts.logfile |> Option.iter (fun f -> f.Dispose())
                                        opts.logfile <- None
                                  )


    /// Sql Transaction wrapper that encapsulates an active database
    /// connection and hands of DynamicSqlCommand objects with the
    /// connection and transcation set correctly
    and DynamicSqlTransaction<'Parameter,
                              'Conn,'Customizer when 'Parameter :> DbParameter
                                    and 'Parameter:(new:unit->'Parameter)
                                    and 'Conn :> DbConnection
                                    and 'Conn:(new:unit->'Conn)
                                    and 'Customizer:(new:unit -> 'Customizer)
                                    and 'Customizer :> Customization<'Parameter,'Conn>
                                    and 'Conn:equality>(conn:DynamicSqlConnection<'Conn,'Parameter,'Customizer>,
                                                        opts:ConnOpts,
                                                        log:string->unit) =
        let baseConn :DbConnection = conn.Take()
        let trans =
            try baseConn.BeginTransaction()
            with _ ->
                conn.Release baseConn
                reraise()
        let mutable disposed = false

        member x.cc commandText =
            let comm = baseConn.CreateCommand()
            comm.CommandText <- commandText
            comm.Transaction<-trans
            new DynamicSqlCommand<'Parameter>(comm,ignore,opts,log)

        member x.Rollback() = trans.Rollback()

        member x.Commit() = trans.Commit()

        interface ISqlConnection with
          member x.Query<'T>(sql, parameters) : seq<'T> =
              x.Query<'T>(sql, ?parameters=parameters)
          member x.InsertMany<'T,'R>(items, table, ignoredColumns) =
              x.InsertMany<'T,'R>(items, ?table=table, ?ignoredColumns=ignoredColumns)
          member x.InsertOne<'T,'R>(item, table, ignoredColumns) =
              x.InsertOne<'T,'R>(item, ?table=table, ?ignoredColumns=ignoredColumns)
          member x.ExecuteScalar(sql, parameters) =
              x.ExecuteScalar(sql, ?parameters=parameters)
          member x.ExecuteNonQuery(sql, parameters) =
              x.ExecuteNonQuery(sql, ?parameters=parameters)

        member x.Query<'T>(sql:string, ?parameters:seq<string*obj>) : seq<'T> =
            conn.Query<'T>(sql, transProvided=x, ?parameters=parameters)

        member x.InsertMany<'T,'R> (items : 'T seq,?table:string,?ignoredColumns: string seq) =
            conn.InsertMany<'T,'R>(items, ?table=table, transProvided=x, ?ignoredColumns=ignoredColumns)

        member x.InsertOne<'T,'R> (item : 'T,?table:string,?ignoredColumns:string seq) =
            conn.InsertMany<'T,'R>([item], ?table=table, transProvided=x, ?ignoredColumns=ignoredColumns).[0]

        member x.ExecuteScalar(sql:string, ?parameters:seq<string*obj>) =
            use comm = x.cc sql
            addParameters comm parameters
            comm.ExecuteScalar()

        member x.ExecuteNonQuery(sql:string, ?parameters:seq<string*obj>) =
            use comm = x.cc sql
            addParameters comm parameters
            comm.ExecuteNonQuery()

        interface IDisposable with
            member x.Dispose() =
                if not disposed then
                    disposed <- true
                    try trans.Dispose()
                    finally conn.Release baseConn
