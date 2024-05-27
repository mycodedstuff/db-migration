<h1 style="display:flex"> <image src="https://skillicons.dev/icons?i=haskell" width=36 style="margin-right:10px"> db-migration</h1>

![Version](https://img.shields.io/badge/version-v0.0.1-blue)
[![Haskell CI](https://github.com/mycodedstuff/db-migration/actions/workflows/haskell.yml/badge.svg)](https://github.com/mycodedstuff/db-migration/actions/workflows/haskell.yml)
![GHC Version](https://img.shields.io/badge/GHC-v8.10.7-brightgreen)
![Last Commit](https://img.shields.io/github/last-commit/mycodedstuff/db-migration/main)

![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)
![Postgres](https://img.shields.io/badge/postgres-%23316192.svg?style=for-the-badge&logo=postgresql&logoColor=white)
### A Haskell library to generate DDL queries by analyzing diff between Haskell and Database schema based on [beam](https://github.com/mycodedstuff/beam)


#### How can this library help?

1. Generate SQL statements for a given schema (trick is to run it against an empty/undefined schema)
2. Validate if database schema is in sync
3. Generate delta DDL statement for quick sync (dev productivity)
4. Automatic schema sync on runtime (pass the generated sql statements to database)

#### Usage
Module Database.Migration exports a function schemaDiff which takes a Connection (postgresql-simple Connection type), schema name and a CheckedDatabaseSetting which returns either DB is in sync or a list of DDL statements.

Refer to [examples](examples) for implementation

Example output:
```sql
❯ stack run
Initiating connect
Connected to postgres
create schema if not exists migration;
create sequence if not exists migration."Configurations_id_seq" as bigint increment by 1 minvalue 1 maxvalue 9223372036854775807 start with 1;
create table if not exists migration."Configurations" (id bigint not null primary key default nextval('migration."Configurations_id_seq"'::regclass), key varchar not null, value varchar not null, "createdAt" timestamp with time zone not null, "updatedAt" timestamp with time zone not null);
create table if not exists migration."Issues" (id varchar not null primary key, message varchar not null, status "enum_Issues_status" not null, image bytea, store json, "createdAt" timestamp with time zone not null, "updatedAt" timestamp with time zone not null);
```

#### Additional Features
##### 1. Lenient type checks for columns

This library allows you to define which type differences are acceptable using option `typeLenient`.
This options takes a function with 2 parameters column type defined in haskell schema, data type in postgres and result true if the difference is acceptable.
> This is useful in case where you already have a database whose column types may not agree with beam and running alters may not be feasible

Sample Usage
```haskell
-- Define a function which returns true for acceptable type differences
-- Below function says if haskell schema has Text and DB has Varchar (any length) then it's acceptable and vice versa for other cases it's not
columnTypeLenient :: ColumnType -> ColumnType -> Bool
columnTypeLenient PgText (VarChar _) = True
columnTypeLenient (VarChar _) PgText = True
columnTypeLenient _ _ = False

-- Supply this function to schemaDiff via options
schemaDiff conn dbSettings $ defaultOptions {typeLenient = Just columnTypeLenient}
```
> Refer [this](#haskell-and-postgres-type-mapping-with-db-migration) table to understand type for lenient type matching

##### 2. Table Partition Support

Option `partitionOptions` can be used to provide partition names for a particular table.
When supplied this with option the mentioned partitions will be included as separate tables and will share the schema of the parent table name supplied in key

> Note: Currently both the parent table `Issues` and it's partitions will be included in validation. This option is useful for application managed partitioning.

Sample Usage
```haskell
-- Define the partitions against a table name
-- In below sample there is an Issues table with 3 partitions by month
partitions :: HM.HashMap Text [Text]
partitions =
  HM.fromList
    [("Issues", ["Issue_M_202405", "Issue_M_202404", "Issue_M_202403"])]


-- Supply this map to schemaDiff via options
schemaDiff conn dbSettings $ defaultOptions {partitionOptions = DBM.PartitionOption True partitions}
```

#### Haskell and Postgres type mapping with db-migration
| Haskell | Postgres | db-migration (ColumnType) |
| --- | --- | --- |
| Text    |  varchar |  Varchar     |
| Char    |  char    |  Char        |
| String  |  varchar |  Varchar     |
| Integer |  bigint  |  BigInt      |
| Int16   |  smallint | SmallInt    |
| Int32   |  integer |  Integer     |
| Int64   |  bigint  | BigInt       |
| Scientific | numeric | Numeric    |
| ByteString | bytea |  Bytea       |
| LocalTime | timestamp | Timestamp |
| UTCTime | timestamp with time zone | Timestamp |
| Value   | json     |  JSON        |
| Value   | jsonb    |  JSONB       |
| Bool    | bool     | Boolean      |
| Double  | double   | Double       |
| [Text]  | varchar()[] | Arr Varchar |
| T = A \| B | enum    | Enum         |

#### TODOs

1. Support for postgres partitioned tables

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

