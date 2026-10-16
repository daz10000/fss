# Samples

Each sample is a small console app that references the library source (`ProjectReference`). In your own
project, use the `PackageReference` shown in the comment in each `.fsproj`. Every sample exits non-zero
on failure, and CI runs them as smoke tests.

| Sample | Shows | Run |
|---|---|---|
| [DuckDbQuickStart](DuckDbQuickStart/Program.fs) | The whole core API: records, inserts with generated keys, parameters, scalars and tuples, transactions, `ISqlConnection` | `dotnet run --project samples/DuckDbQuickStart` |
| [DuckDbAnalytics](DuckDbAnalytics/Program.fs) | Querying CSV and Parquet in place, aggregates and window functions into records, persisting to a `.duckdb` file | `dotnet run --project samples/DuckDbAnalytics` |
| [SqliteTodo](SqliteTodo/Program.fs) | A tiny CLI app on a SQLite file; type conversion (`bool` ↔ 0/1) | `dotnet run --project samples/SqliteTodo -- add "milk"` |
| [SqliteToDuckDb](SqliteToDuckDb/Program.fs) | ETL with one record type across two databases (and two fss packages in one project) | `dotnet run --project samples/SqliteToDuckDb` |
| [PostgresQuickStart](PostgresQuickStart/Program.fs) | Schemas, serial keys, `@name` parameters. Needs `FSS_PG` set to a connection string. | `FSS_PG="Host=...;..." dotnet run --project samples/PostgresQuickStart` |
| [LegacyWebServer](LegacyWebServer/Program.fs) | The legacy `fss` HTTP server and templates, with escaping done properly | `dotnet run --project samples/LegacyWebServer` |
| [scripts/duckdb-query-csv.fsx](scripts/duckdb-query-csv.fsx) | Using `fss_duckdb` from an F# script | `dotnet fsi samples/scripts/duckdb-query-csv.fsx` |
