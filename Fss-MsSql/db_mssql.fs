namespace Fss.Data

/// Wrapper around MsSql database connection using dynamic operators, plus some other useful utils (e.g. ParameterArray). 
/// Most of the code here comes from Thomas Petricek's original use of dynamic operator for SqlClient.
/// See http://tomasp.net/blog/dynamic-sql.aspx/

module MsSql =
    open System
    open System.Data
    open System.Data.SqlClient
    open System.Collections.Generic

    
    type DynamicMsSqlDataReader(reader:SqlDataReader) =
      member private x.Reader = reader
      member x.Read() = reader.Read()
      static member (?) (dr:DynamicMsSqlDataReader, name:string) : 'R = 
        unbox (dr.Reader.[name])
      interface IDisposable with
        member x.Dispose() = reader.Dispose()

    type DynamicMsSqlCommand(cmd:SqlCommand) = 
      member private x.Command = cmd
      static member (?<-) (cmd:DynamicMsSqlCommand, name:string, value) = 
        cmd.Command.Parameters.Add(SqlParameter("@" + name, box value)) |> ignore
      member x.ExecuteNonQuery() = cmd.ExecuteNonQuery()
      member x.ExecuteReader() = new DynamicMsSqlDataReader(cmd.ExecuteReader())
      member x.ExecuteScalar() = cmd.ExecuteScalar()
      member x.Parameters = cmd.Parameters
      /// Adds an array of parameters in the SQL query. 
      /// The SQL query in the should contain a {paramNameRoot} element that will be expanded
      /// with as many parameters as there are elements in the input values collection. 
      /// For example, if there are three elements in values, the {paramNameRoot} element will be expanded
      /// to @paramNameRoot1, @paramNameRoot2, @paramNameRoot3 and each each of these parameter will be bound
      /// to a value from the input values list. 
      /// This is especially handy for IN statements:
      ///    SELECT * FROM table WHERE name IN ({mynames})
      /// Use AddArrayParameters(myNames, "mynames")  to bind the values in myNames to parameters in {mynames}
      /// Default start is 1 and default separator is ", "
      member x.AddArrayParameters(values : seq<'T>,  paramNameRoot: string, ?start, ?separator) = 
        let start = 
            match start with
            | Some start -> start
            | None -> 1
        let separator = 
            match separator with
            | Some separator -> separator
            | None -> ", "
         (* An array cannot be simply added as a parameter to a SqlCommand so we need to loop through things and add it manually. 
             * Each item in the array will end up being it's own SqlParameter so the return value for this must be used as part of the
             * IN statement in the CommandText.
            Code directly adapted from C# implementation on http://stackoverflow.com/a/2377651 (hence the imperative style..)
         *)
        let parameters = new List<SqlParameter>()
        let parameterNames = new List<string>()
        let mutable paramNbr = start
        for value in values do
            let paramName = sprintf "@%s%d" paramNameRoot paramNbr
            paramNbr <- paramNbr + 1
            parameterNames.Add(paramName)
            parameters.Add(cmd.Parameters.AddWithValue(paramName, value))
        cmd.CommandText <- cmd.CommandText.Replace("{" + paramNameRoot + "}", String.Join(separator, parameterNames))
        parameters.ToArray()
    
      interface IDisposable with
        member x.Dispose() = cmd.Dispose()

    type DynamicMsSqlConnection(conn:SqlConnection) =
        do
            conn.Open()
    
        member private x.Connection = conn

        member x.Command(commandText:string) =
            use comm = conn.CreateCommand()
            comm.CommandText <- commandText
            //match trans with | None -> () | Some(t) -> comm.Transaction <- t
            new DynamicMsSqlCommand(comm)

        new (connStr:string) = 
            new DynamicMsSqlConnection(new SqlConnection(connStr))
        interface IDisposable with
            member x.Dispose() = 
                conn.Close()
                conn.Dispose()
