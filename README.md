# db-migration

### A Haskell library to generate DDL queries by analyzing diff between Haskell and Database schema based on [beam](https://github.com/mycodedstuff/beam)

#### How can this library help?

1. Generate SQL statements for a given schema (trick is to run it against an empty schema)
2. Validate if database schema is in sync
3. Generate delta DDL statement for quick sync (dev productivity)
4. Automatic schema sync on runtime (pass the generated sql statements to database)

#### Usage
Module Database.Migration exports a function schemaDiff which takes a Connection (postgresql-simple Connection type), schema name and a CheckedDatabaseSetting which returns either DB is in sync or a list of DDL statements.

Refer to [examples](examples) for implementation

#### TODOs

1. Support for default values
2. Support for sequences
3. Support for partitioned tables (manual and db supported)

#### Explore

1. Define, identify and sync indexes
2. Schema specific enums
3. Report columns defined in database not in Haskell schema
4. DROP DDL statements

#### Notes

1. Currently this library only supports PostgreSQL database
2. Currently this library only validates if Haskell schema is defined in database. Database may have more tables and columns
3. This library uses a fork of [beam](https://github.com/mycodedstuff/beam) which has some additional fixes/improvements (You can find the original repo [here](https://github.com/haskell-beam/beam))

