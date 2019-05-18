{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "time-table"
, dependencies =
    [ "bouzuya-datetime-formatter"
    , "node-fs"
    , "node-process"
    , "psci-support"
    , "test-unit"
    , "yargs"
    ]
, packages =
    ./packages.dhall
}
