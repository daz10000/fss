namespace Fss.Data

open System.IO
open System.Data
open MySql.Data.MySqlClient


module MySql = 
    type DynamicSqlConnection = Fss.Data.Common_mysql.DynamicSqlConnection<MySqlConnection,MySqlParameter>
    type DynamicSqlTransaction = Fss.Data.Common_mysql.DynamicSqlTransaction<MySqlParameter,MySqlConnection>

