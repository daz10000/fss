### v2.0.0 -- Jan 12th 2025
* MNT: ported to net8
* FEA: duckdb support

### v1.7.0 -- Jun 11th 2019
* MNT: ported to netcore and fake5

### v1.6.1 -- Sep 13th 2018
* ENH: improvements to error handler

### v1.6.0 -- Sep 12th 2018
* FEA: optional error handler for server 

### v1.5.8 -- Jul 3rd 2017
* FEA: added secure UD implementation

### v1.5.7 -- Jun 28 2017
* BUG: post requests did not handle ? style arguments gracefully
* BUG: rare race condition between dispose and keep alive could throw exception
* MNT: updated project files to build under VS2017 on command line cleanly

### v1.5.6 -- Mar 13th 2017
* MNT: updated to FSharp.Core 4.4.1.0

### v1.5.5 -- Feb 5th 2017
* BUG: postgres db reopening could throw error and die previously
* LNT: cleaned up fslint recommendations for template.fs
* FEA: moved exposed calculation routines in template to make them useful for external applications

### v1.5.4 -- Jan 08 2017
* MNT: update to 3.1.9.0 Npgsl

### v1.5.3 -- Nov 16 2016
* ORM: fss_postgres reconnect stability improvements

### v1.5.2 -- Sep 05 2016
* ORM: mapping metadata to lowercase as sqlite does not match postgres behavior

### v1.5.1 -- Aug 30 2016
* ORM: specified explicit schema now for insertMany

### v1.5.0 -- Aug 17 2016
* ORM: support for sqlite solid
* ORM: support for postgres enums solid
* ORM: support for postgres schemas / search_path

### v1.4.1 -- Aug 7 2016
* ORM: support for sqlite added
* ORM: small cleanup issue in keepalive thread fixed

### v1.4.0 -- July 17 2016
* ORM: caching schema information at first load for postgres
* ORM: better NULL/ Nullable support

### v1.3.1
* ORM: int16,decimal support
* templates: expression legal in for loop iter RHS value
* templates: elseif implementation

### v1.3.0
* ORM: fss-mysql implementation
* ORM: decimal null support

### v1.2.9
* templates: BUG: includes with internal variables mixed with extends werenot expanded properly

### v1.2.8
* server: BUG empty multipart form parts where triggering assertions incorrectly

### v1.2.7
* server: BUG: rewrote multipart header parser to be tail recursive

### v1.2.6
* Freql:  methods added directly to transaction object to make it behave more like a connection

### v1.2.5
* Freql: premature release of transaction connection fixed
* Server: reexposed Fss.Server.version string
* templates:  if x.y case fixed and tested

### v1.2
* Freql:  bug in Query led to delayed connection pool release for incomplete seq  consumption
* templates: ENH - range expressions for for loops implemented
* templates: ENH - expressions allowed within double parenthesis substitution blocks
* templates: ENH - array indices implemented for expressions
* templates: ENH - <= and >= implemented

### v1.1.5
* templates: overhaul of operator precedence to implement boolean expressions correctly
* Freql:  handling nullable date fields implemented.

### v1.0.6
* templates: bug in double quoted string parsing fixed string  hello not recognized :(
* templates: != operator implemented for '{% if %}'
* test:  additional unit tests for string , != operations
* templates: == instead of = for equals comparison (for consistency with standard jinja2 docs)
* templates: single variable to if '{% if foo %}' will test whether variable is set or has a non false value ('""' and [||] considered false)
* fss: added support for 302, 303, 304, and 307 response codes and http303 redirect helper
* fss: added overridable preCheckFailed to UD and ability to modify url path within precheck

### v1.0.5
* Freql: Support for null fields in tables, mapped onto option fields in records
* templates: changed extends -> include in jinja2 clone to correctly reflect jinja semantics
* templates: implemented jinja2 blocks
* templates: implemented jinja2 extend
* templates: fixed empty for/if blocks
* Added unit tests for pg_db.fs
* Added unit tests for template extends and blocks cases

### v1.0.2
* Bug fix for premature dbconnection returning to pool

### v1.0.1
* Added logging for queries (optionally conditional on runtime)
* Added logging for connection pool usage
* Killed bug due to premature release of db connection
* Added support for ignoredColumns argument to InsertMany for db supplied column values

### v1.0.0
* Added external transaction support to FreQL InsertMany
* Added error message for table not found on FreqInsert
* Added typed URL dispatch functions to catch errors at edit time
*    Instead of box myFunc   , use  D0 myFunc  (or D1 myFunc if it takes 1 param etc, D2.. D7 are defined)

### v0.9.0
* Added FreQL - F# Record / SQL support under Fss.Data.Postgres

### v0.8.0
* Fixed bug with returning unicode text strings that was causing a discrepancy in content length and actual # of bytes.
* Added /index.html page to ease access to example cases.

### v0.7.1
* Added mutex around thread tracking to prevent concurrent thread addition during iteration for shutdown

### v0.7.0
* Cleaned up some critical network/streamwriter buffer flushing bugs that could under some circumstances create issues

### v0.6.4
* Added Stop() method to shut down server

### v0.6.3
* Recursive application of variable expansion for included header files
* Fixes to form argument parsing


### v0.6.0
* Added simple pooling system to help apps manage connection pools etc

### v0.5.8
* replaces error message with "none" for null values in template fields

### v0.5.7
* fix for post streams lingering on auth failure
* int64 ICONST64 template implementation

### v0.5.6
* '{% else %}' implemented

### v0.5.5
* not operator implemented
* template extensions working - see /template1 example in FssExample


### v0.5.4
* Bug fix for dot notation references in local variables

### v0.5.3
* Better logging for error reporting
* Fixes for single '{ and }' characters in templates

### v0.5.0
* Major clean up of template library, bug fixes and addition of many tests

### v0.4.5
* Added GET member and parsing of ? arguments after GET calls.


### v0.4.0
* Hook added to allow security/session implementations on top of Fss.UD
* Prototype JSON library included, allows deserialization,  
* objects not implemented yet
* AddDispatcher method added to UD to allow later addition to the dispatch URLS
* Template system moved into Template module from Playground, basically working

### v0.3.0
* removed non tail recursive code in main dispatch loop
* extended experimental templating system to parse and handle for loops.  Not complete yet.

### v0.1.1
* added support for user addition of custom headers and a JPEGResponse type.  Added example imgDownload() method

### v0.1.0
* added ReadPostAsBytes() ReadPostAsString() methods to assist pulling back post data.  
* added assertion example

### v0.0.0
* basic web server framework released
