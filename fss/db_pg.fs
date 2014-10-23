namespace Fss.Data

/// Postgres database wrapper.  Independent of other Fss pieces,
/// can be omitted along with System.Data and NPgsql dependencies for a smaller compilation unit
/// or used standalone from other Fss pieces.
/// Credit to Thomas Petricek for the original dynamic operator concept 
open System.IO
open Npgsql
open System.Data

module Postgres =
    type DynamicSqlConnection = Fss.Data.Common.DynamicSqlConnection<NpgsqlConnection,NpgsqlParameter>

