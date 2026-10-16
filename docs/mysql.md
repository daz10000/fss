# MySQL (`fss_mysql`): retired

`fss_mysql` is **retired**. Version 2.0.0 stays on NuGet for existing users, but new versions are not
published and the package isn't tested in CI.

Its implementation (`src/fss/db_common_mysql.fs`) is an older fork of the shared core. It lacks the
features and fixes in the other packages: parameterised `Query`, type conversion, scalar and tuple
queries, schema-qualified inserts, and the pool and transaction fixes. Its key retrieval assumes
Postgres-style `RETURNING`, which MySQL doesn't support.

For new MySQL work, the options are:

- DuckDB's MySQL extension, to query MySQL from `fss_duckdb`:
  `install mysql; load mysql; attach 'host=... user=... database=...' as my (type mysql)`
- Porting `fss_mysql` onto the shared core (`db_common.fs`) with `MySqlConnector`. That's a moderate
  piece of work: write a `Customization` (see `src/fss_sqlite/db_sqlite.fs` for the smallest example)
  that uses `information_schema` for metadata and `select last_insert_id()` for keys.

`MySql.Data` is licensed GPL-2.0 with the Universal FOSS Exception. `MySqlConnector` is MIT.
