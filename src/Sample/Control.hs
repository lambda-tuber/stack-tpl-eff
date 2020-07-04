{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Sample.Control (
    ArgData(..)
  , run
  ) where

import qualified Control.Exception.Safe as E
import Data.Default
import Data.Yaml
import Control.Lens

import Sample.Type
import qualified Sample.Utility as U
import qualified Sample.Application as A

-- |
--
run :: ArgData -> IO ()
run args = do

  conf <- maybe (pure def) decodeFileThrow (args^.yamlArgData)

  U.setupLogger (conf^.logLevelConfigData) (conf^.logFileConfigData)

  let appDat = AppData {
      _logLevelAppData = conf^.logLevelConfigData
    }

  U.runApp appDat A.app >>= \case
    Right _ -> return ()
    Left er -> E.throwString er

