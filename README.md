fss
===

FSharp Server - lightweight web server

# Getting started

## Install fake command line tool

You can omit -g if you don't want it globally

`dotnet tool install fake-cli -g`


## Run build script

`fake run`



## Developers


### Example `connection_postgres.txt`

Tests require a postgres connection string in `tests/fss.Tests/connection_postgres.txt`
`User ID=username;Password=password;Host=localhost;Port=5432;Database=testfss`

### Example sqlite connections string

Tests require a sqlite connection string in `tests/fss.Tests/connection_sqlite.txt`

`Data source=test.db`
