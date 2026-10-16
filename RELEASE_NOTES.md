# Release Notes

## 2.1.0 - 2026-10-17

Packages: fss_duckdb, fss_postgres, fss_sqlite, fss. All packages now share one version number.
fss_mysql is retired and not published.

### Bug fixes
* BUG: fss_duckdb `InsertOne`/`InsertMany` never worked: the column metadata query returned nothing,
  generated SQL used `:p1` parameters that DuckDB rejects, and opening a database file containing a
  primary key threw `InvalidCastException`. Metadata now comes from `duckdb_columns()` and `duckdb_constraints()`.
* BUG: fss_duckdb gave each pooled connection to `:memory:` its own private database. Pooled connections now share one.
* BUG: fss_sqlite threw on every connection after the move to Microsoft.Data.Sqlite (`GetSchema("Columns")` is
  not supported there). Metadata now comes from `pragma_table_info`.
* BUG: `Query` took a pooled connection before the sequence was enumerated, leaking it if the sequence was never
  read and breaking re-enumeration.
* BUG: an exception partway through `InsertMany` left its transaction open and the connection checked out of the
  pool. It now rolls back and releases the connection.
* BUG: `Pool.Release` of an item that was already free looped forever while holding the pool lock (Release builds).
  It now throws `InvalidOperationException`. The pool also no longer loses wakeups under contention.
* BUG: the stored-procedure operator `conn?proc` returned a command that had already been disposed.
* BUG: the keep-alive thread was a foreground thread, so a forgotten `Dispose` stopped the process from exiting.
  It never runs for DuckDB or SQLite.
* BUG: templates evaluated `a and b and c` as `a and c`.
* BUG: web server: a `Response.Cookie` was sent as a `Cookie` header instead of `Set-Cookie`; error responses
  never terminated their headers, so keep-alive clients hung; `halt()` threw on .NET 5+ (`Thread.Abort`); and the
  accept loop grew the stack by one frame per connection.
* BUG: `Cookie.formatCookieDate` printed the month in the minutes field.
* BUG: multipart parsing failed when the closing boundary had no trailing CRLF.
* BUG: fss_mysql `Query` built a record after every field instead of once per row.

### Security
* SEC: `Sec.genRandKey` used `System.Random` (predictable, not thread-safe). It now uses `RandomNumberGenerator`.
* SEC: the web server rejects CR/LF in response headers (response splitting), and the default error page
  HTML-encodes exception text.
* SEC: documented that `Cookie.checkCookie` is not safe for authentication and that templates don't escape output.

### Features
* FEA: parameterised queries: `Query`, `ExecuteScalar` and the new `ExecuteNonQuery` take `[ "name", box value ]`.
* FEA: `Query<'T>` maps to tuples (by position) and single values (`Query<int64> "select count(*) ..."`), not
  only records. Option fields missing from the result become `None`.
* FEA: values are converted between compatible types (e.g. int64 → int, 0/1 → bool, HUGEINT → int64,
  DATE ↔ DateOnly, text → Guid / F# enum, BLOB stream → byte[]), with clear errors naming the column.
* FEA: inserts accept schema-qualified table names (`table = "sales.orders"`), and a table created after the
  connection opened is found without calling `Reload()`.
* FEA: fss_postgres `registerDataSource` configures Npgsql type mappings (enums, plugins), replacing the
  `MapEnumGlobally` that Npgsql removed.

### Breaking changes (source compatible for typical use)
* `Query`'s optional arguments are now `(sql, ?parameters, ?transProvided)`. Code passing `transProvided` by name
  is unaffected.
* `ISqlConnection.Query`/`ExecuteScalar` gained an optional `parameters` argument, and `ISqlConnection` gained
  `ExecuteNonQuery`. Only custom implementations of `ISqlConnection` need changes.
* `Customization` (the dialect interface) gained `openConnection` and `parameterPrefix`.
* Reading NULL into a non-option value-type field now throws a descriptive error instead of an `ArgumentException`.

### Maintenance
* MNT: .NET 10 only, NuGet Central Package Management; Paket and FAKE removed; `fss.slnx` solution.
* MNT: updated dependencies: DuckDB.NET 1.5.6.1, Npgsql 10, Microsoft.Data.Sqlite 10, NUnit 5. Removed the unused
  Newtonsoft.Json dependency.
* MNT: SourceLink, deterministic builds, symbol packages, package readmes, and the license as an SPDX expression.
* MNT: GitHub Actions CI (Linux with Postgres, Windows, macOS) and NuGet Trusted Publishing for releases.
* TST: DuckDB test suite; Postgres/MySQL tests skip unless `FSS_TEST_POSTGRES` / `FSS_TEST_MYSQL` are set.
* DOC: new documentation in `docs/`, samples in `samples/`, `MAINTAINERS.md`.
## 2.0.0 - 2025-01-13
* MNT: ported to net8
* FEA: duckdb support

## 1.7.0 - 2019-06-11
* MNT: ported to netcore and fake5

## 1.6.1 - 2018-09-13
* ENH: improvements to error handler

## 1.6.0 - Sep 12th 2018
* FEA: optional error handler for server 

## 1.5.8 - Jul 3rd 2017
* FEA: added secure UD implementation

## 1.5.7 - Jun 28 2017
* BUG: post requests did not handle ? style arguments gracefully
* BUG: rare race condition between dispose and keep alive could throw exception
* MNT: updated project files to build under VS2017 on command line cleanly

## 1.5.6 - Mar 13th 2017
* MNT: updated to FSharp.Core 4.4.1.0

## 1.5.5 - Feb 5th 2017
* BUG: postgres db reopening could throw error and die previously
* LNT: cleaned up fslint recommendations for template.fs
* FEA: moved exposed calculation routines in template to make them useful for external applications

## 1.5.4 - Jan 08 2017
* MNT: update to 3.1.9.0 Npgsl

## 1.5.3 - Nov 16 2016
* ORM: fss_postgres reconnect stability improvements

## 1.5.2 - Sep 05 2016
* ORM: mapping metadata to lowercase as sqlite does not match postgres behavior

## 1.5.1 - Aug 30 2016
* ORM: specified explicit schema now for insertMany

## 1.5.0 - Aug 17 2016
* ORM: support for sqlite solid
* ORM: support for postgres enums solid
* ORM: support for postgres schemas / search_path

## 1.4.1 - Aug 7 2016
* ORM: support for sqlite added
* ORM: small cleanup issue in keepalive thread fixed

## 1.4.0 - July 17 2016
* ORM: caching schema information at first load for postgres
* ORM: better NULL/ Nullable support

## 1.3.1
* ORM: int16,decimal support
* templates: expression legal in for loop iter RHS value
* templates: elseif implementation

## 1.3.0
* ORM: fss-mysql implementation
* ORM: decimal null support

## 1.2.9
* templates: BUG: includes with internal variables mixed with extends werenot expanded properly

## 1.2.8
* server: BUG empty multipart form parts where triggering assertions incorrectly

## 1.2.7
* server: BUG: rewrote multipart header parser to be tail recursive

## 1.2.6
* Freql:  methods added directly to transaction object to make it behave more like a connection

## 1.2.5
* Freql: premature release of transaction connection fixed
* Server: reexposed Fss.Server.version string
* templates:  if x.y case fixed and tested

## 1.2
* Freql:  bug in Query led to delayed connection pool release for incomplete seq  consumption
* templates: ENH - range expressions for for loops implemented
* templates: ENH - expressions allowed within double parenthesis substitution blocks
* templates: ENH - array indices implemented for expressions
* templates: ENH - <= and >= implemented

## 1.1.5
* templates: overhaul of operator precedence to implement boolean expressions correctly
* Freql:  handling nullable date fields implemented.

## 1.0.6
* templates: bug in double quoted string parsing fixed string  hello not recognized :(
* templates: != operator implemented for '{% if %}'
* test:  additional unit tests for string , != operations
* templates: == instead of = for equals comparison (for consistency with standard jinja2 docs)
* templates: single variable to if '{% if foo %}' will test whether variable is set or has a non false value ('""' and [||] considered false)
* fss: added support for 302, 303, 304, and 307 response codes and http303 redirect helper
* fss: added overridable preCheckFailed to UD and ability to modify url path within precheck

## 1.0.5
* Freql: Support for null fields in tables, mapped onto option fields in records
* templates: changed extends -> include in jinja2 clone to correctly reflect jinja semantics
* templates: implemented jinja2 blocks
* templates: implemented jinja2 extend
* templates: fixed empty for/if blocks
* Added unit tests for pg_db.fs
* Added unit tests for template extends and blocks cases

## 1.0.2
* Bug fix for premature dbconnection returning to pool

## 1.0.1
* Added logging for queries (optionally conditional on runtime)
* Added logging for connection pool usage
* Killed bug due to premature release of db connection
* Added support for ignoredColumns argument to InsertMany for db supplied column values

## 1.0.0
* Added external transaction support to FreQL InsertMany
* Added error message for table not found on FreqInsert
* Added typed URL dispatch functions to catch errors at edit time
*    Instead of box myFunc   , use  D0 myFunc  (or D1 myFunc if it takes 1 param etc, D2.. D7 are defined)

## v0.9.0
* Added FreQL - F# Record / SQL support under Fss.Data.Postgres

## v0.8.0
* Fixed bug with returning unicode text strings that was causing a discrepancy in content length and actual # of bytes.
* Added /index.html page to ease access to example cases.

## v0.7.1
* Added mutex around thread tracking to prevent concurrent thread addition during iteration for shutdown

## v0.7.0
* Cleaned up some critical network/streamwriter buffer flushing bugs that could under some circumstances create issues

## v0.6.4
* Added Stop() method to shut down server

## v0.6.3
* Recursive application of variable expansion for included header files
* Fixes to form argument parsing


## v0.6.0
* Added simple pooling system to help apps manage connection pools etc

## v0.5.8
* replaces error message with "none" for null values in template fields

## v0.5.7
* fix for post streams lingering on auth failure
* int64 ICONST64 template implementation

## v0.5.6
* '{% else %}' implemented

## v0.5.5
* not operator implemented
* template extensions working - see /template1 example in FssExample


## v0.5.4
* Bug fix for dot notation references in local variables

## v0.5.3
* Better logging for error reporting
* Fixes for single '{ and }' characters in templates

## v0.5.0
* Major clean up of template library, bug fixes and addition of many tests

## v0.4.5
* Added GET member and parsing of ? arguments after GET calls.


## v0.4.0
* Hook added to allow security/session implementations on top of Fss.UD
* Prototype JSON library included, allows deserialization,  
* objects not implemented yet
* AddDispatcher method added to UD to allow later addition to the dispatch URLS
* Template system moved into Template module from Playground, basically working

## v0.3.0
* removed non tail recursive code in main dispatch loop
* extended experimental templating system to parse and handle for loops.  Not complete yet.

## v0.1.1
* added support for user addition of custom headers and a JPEGResponse type.  Added example imgDownload() method

## v0.1.0
* added ReadPostAsBytes() ReadPostAsString() methods to assist pulling back post data.  
* added assertion example

## v0.0.0
* basic web server framework released
