module Main where

import System.Exit
import Options.Applicative
import qualified Control.Exception.Safe as E

import Sample.Control

-- |
--
main :: IO ()
main = getArgs >>= \args -> do
       flip E.catchAny exception
     $ flip E.finally  finalize
     $ run args

  where
    finalize = do
      return ()

    exception e = do
      putStrLn "-----------------------------------------------------------------------------"
      putStrLn "ERROR exit."
      print e
      putStrLn "-----------------------------------------------------------------------------"
      exitFailure

-------------------------------------------------------------------------------
-- |
--   optparse-applicative
--
getArgs :: IO ArgData
getArgs = execParser parseInfo

-- |
--
parseInfo :: ParserInfo ArgData
parseInfo = info options $ mconcat
  [ fullDesc
  , header   "This is app program."
  , footer   "Copyright 2020. All Rights Reserved."
  , progDesc "This is app program description."
  ]

-- |
--
options :: Parser ArgData
options = (<*>) helper
  $ ArgData
  <$> confOption

-- |
--
confOption :: Parser (Maybe FilePath)
confOption = optional $ strOption $ mconcat
  [ short 'y', long "yaml"
  , help "config file"
  , metavar "FILE"
  ]

