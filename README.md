## Migration Plus ##

Execute additional actions at [persistent](https://github.com/yesodweb/persistent) migration.

Alpha status... very crude code!

### Motivation ###

TL;DR : Implementation of triggers on [persistent](https://github.com/yesodweb/persistent) [PostgreSQL](http://hackage.haskell.org/package/persistent-postgresql) to enable the [notify/listen](http://www.postgresql.org/docs/9.3/static/sql-notify.html) feature.

Regarding databases (and database abstractions layers), you can either abstract away all the differences in order to be able to migrate your code between backends without difficulties, while sacrificing some of the database features and optimizations, or you could go the other way around.

The first option makes a lot of sense in most cases, but sometimes you find yourself wanting to take advantage of some special features of a given database, and without special requirements for portability.

In my particular case, I wanted to take advantage of [PostgreSQL](http://www.postgresql.org/) [notify/listen](http://www.postgresql.org/docs/9.3/static/sql-notify.html) feature in order to make my code as modular as possible. Of course, one could argue that the same feature could easily be achieved through [other means](http://hackage.haskell.org/package/network-transport), but this assumes all components of the system will be Haskell, which may or may not be the case. But I digress... 

[Persistent](https://github.com/yesodweb/persistent) currently lacks the means to run custom migration at startup. One could always run a [rawSql](http://hackage.haskell.org/package/persistent-1.2.3.0/docs/Database-Persist-Sql.html#v:rawSql) command afterwards, but wouldn't a syntax like this:

    LowerCaseTable id=my_id
        fullName Text
        Triggers
            tableIdTrig AFTER INSERT
            tableTrig BEFORE DELETE
        Indexes
            tableIndex full_name

be much nicer?

### Current Status ###

The code currently supports:

* **Triggers** (PostgreSQL, MySQL, SQLite)
* **Indexes** (Sqlite)

A **trigger** can be:

* Triggered [BEFORE|AFTER] an event.
* Only row triggers supported at the moment (statement triggers seem trickier).
* A [plpgsql](http://www.postgresql.org/docs/9.3/static/plpgsql.html) function for PostgreSQL, compile time checked throught the [hssqlppp](http://jakewheat.github.io/hssqlppp/) package. In the [example](https://github.com/jcristovao/migrationplus/blob/master/test/PgsqlSql.hs) code we test both PostgreSQL notification and a simple update.
 + Supports multiple trigger events \[INSERT|UPDATE|DELETE|TRUNCATE\] simultaneously (separated by OR).
* A SQL statement for MySQL and SQLite. Please note that MariaDB (the tested backend) does not seem to support updates on the same entry (table?) that raised the trigger.
 + Supports the following trigger events \[INSERT|UPDATE|DELETE\] (just one at a time).

Both the Trigger specification (on the persistent entity declaration) and the SQL Code are verified at compile time through Template Haskell and Quasiquotation hackery. If you despise these techniques, this package is probably not for you :P

This, however, has some consequences due to the [GHC Stage Restriction](http://stackoverflow.com/questions/18913078/can-a-quasiquoter-use-variables-defined-in-the-same-file-it-is-called). Namely, you should declare your SQL in a separate file, and then import it in a `MigrationSql.hs` file.

This seems a bit messy, and well, quite frankly, it is. But it works. Thus:

A) Declare all your sql functions in a file, for example `SqliteSql.hs`:

    {-# LANGUAGE QuasiQuotes #-}
    {-# LANGUAGE TemplateHaskell #-}
    {-# LANGUAGE ScopedTypeVariables #-}
    {-# LANGUAGE OverloadedStrings #-}
    module SqliteSql where
    
    import Text.Shakespeare.Text
    import Database.Persist.Migrationplus
    
    tableIdTrig :: SqlUnit
    tableIdTrig = ("tableIdTrig",[lt|
          INSERT INTO ref_table (something_else,lct) SELECT my_id,1 FROM lower_case_table ORDER BY my_id DESC LIMIT 1;
        |])
    
    tableTrig :: SqlUnit
    tableTrig = ("tableTrig",[lt|
          UPDATE lower_case_table SET full_name = "abc" WHERE id = 1;
        |])
    
    sql :: [SqlUnit]
    sql = [tableTrig, tableIdTrig]
    
Don't forget to gather around all defined variables in one list, that in this case I (on a very imaginative way) called `sql`. 
The first element of a tupple should be the trigger name defined in the entity declaration, or you will run into a compilation error.
 Since PostgreSQL uses functions for triggers, the PostgreSQL syntax is somewhat simplified (instead of tupples you just have Lazy Text quasiquotations),
 and thus it is the (plpgsql) function name that must match the trigger name in the entity definition.

B) Collect the SQL and define the custom `persistWith` functions. This is a step necessary to get compile time validation of the SQL:

    {-# LANGUAGE CPP #-}
    module MigrationSql
      ( persistLowerWithSql
      , persistUpperWithSql
      , module Database.Persist.Sqlite.Migrationplus
      , module SqliteSql
      ) where
    
    import Database.Persist.Sqlite.Migrationplus
    import SqliteSql
    
    persistLowerWithSql = persistL sql
    persistUpperWithSql = persistU sql

C) Run your migration code with one of these functions:

    import MigrationSql
    (...)
    share [mkPersist sqlSettings, mkMigrate "lowerCaseMigrate"] [persistLowerWithSql|
    
D) Use one of the custom functions defined in this package to replace the backend specific function:

    withPostgresqlPool' sql pgconn 1 $ (...)

An **index** is defined as:

An index name followed by a list of columns to index. Sub-indexes are currently not supported.

Actually, index support is really very limited, mainly due to the lack of support of the `IF NOT EXISTS` in either MySQL or PostgreSQL.
Each one of these databases requires the use of functions to conditionally create an index (meaning, only create if it does not exist),
which currently cannot be achieved without further hackery directly into the persistent source code.

### Limitations ###

* Depends on modified version of persistent. I kept the modifications to a minimum, hoping they will be accepted...
* Trigger and Index names are not escaped (not so serious as it may sound, since no user input should ever reach here, but a serious limitation nevertheless)
* Index support implies further (more extensive) modifications to persistent.
* Uses template haskell and thus its impacts on the compilation speed.
* Dependes on [hssqlppp](http://jakewheat.github.io/hssqlppp/), which currently supports only PostgreSQL, Oracle and MS-SQLServer (no explicit MySQL or SQLite) support. However, I found that Jack Wheat seems quite open to patches and contributions, so that could change (and his package supports 'generic' SQL quite well).
* Code is not mature...

Any contribution or suggestions is most welcomed!
