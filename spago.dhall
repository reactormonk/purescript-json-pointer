{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "json-pointer"
, dependencies =
    [ "console"
    , "effect"
    , "foreign"
    , "generics-rep"
    , "prelude"
    , "psci-support"
    , "spec-quickcheck"
    , "strings"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
