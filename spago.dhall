{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "time-table"
, dependencies =
    [ "bouzuya-datetime"
    , "console"
    , "effect"
    , "formatters"
    , "node-process"
    , "psci-support"
    , "test-unit"
    , "yargs"
    ]
, packages =
    ./packages.dhall
}
