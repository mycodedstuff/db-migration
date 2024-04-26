# <image src="https://wiki.haskell.org/wikistatic/haskellwiki_logo.png?e89e3" width=38> db-migration
![Version](https://img.shields.io/badge/version-v0.0.1-blue)
[![Haskell CI](https://github.com/mycodedstuff/db-migration/actions/workflows/haskell.yml/badge.svg)](https://github.com/mycodedstuff/db-migration/actions/workflows/haskell.yml)
![GHC Version](https://img.shields.io/badge/GHC-v8.10.7-brightgreen)
![Last Commit](https://img.shields.io/github/last-commit/mycodedstuff/db-migration/main)

![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)
![Postgres](https://img.shields.io/badge/postgres-%23316192.svg?style=for-the-badge&logo=postgresql&logoColor=white)
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

#### Tested Platforms
![macOS](https://img.shields.io/badge/mac%20os-000000?style=for-the-badge&logo=macos&logoColor=F0F0F0)
![Ubuntu](https://img.shields.io/badge/Ubuntu-E95420?style=for-the-badge&logo=ubuntu&logoColor=white)

