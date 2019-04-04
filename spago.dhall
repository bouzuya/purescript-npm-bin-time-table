{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "time-table"
, dependencies =
    [ "bouzuya-datetime"
    , "bouzuya-datetime-formatter"
    , "console"
    , "effect"
    , "formatters"
    , "node-fs"
    , "node-process"
    , "psci-support"
    , "test-unit"
    , "yargs"
    ]
, packages =
    ./packages.dhall
}
