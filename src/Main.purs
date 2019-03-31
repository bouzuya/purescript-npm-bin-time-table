module Main
  ( main
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception as Exception
import Node.Yargs.Applicative as Yargs
import Node.Yargs.Setup as YargsSetup

app :: String -> Effect Unit
app "15min" = Console.log "15min!"
app line = Exception.throw (line <> " is not supported")

main :: Effect Unit
main = do
  let setup = YargsSetup.usage "Usage: $0 "
  Yargs.runY setup do
    app <$> Yargs.yarg "line" [] (Just "line") (Left "15min") true
