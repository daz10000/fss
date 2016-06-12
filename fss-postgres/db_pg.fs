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
        interface Fss.Data.Common.Customization<NpgsqlParameter> with
            member x.makeEnum name v =
                let p = Npgsql.NpgsqlParameter(name,v)
                p.NpgsqlDbType<-NpgsqlTypes.NpgsqlDbType.Unknown
                p
    end

    type ISqlConnection = Fss.Data.Common.ISqlConnection
    type DynamicSqlConnection = Fss.Data.Common.DynamicSqlConnection<NpgsqlConnection,NpgsqlParameter,PgCustomizations>
    type DynamicSqlTransaction = Fss.Data.Common.DynamicSqlTransaction<NpgsqlParameter,NpgsqlConnection,PgCustomizations>

