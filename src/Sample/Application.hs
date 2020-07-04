{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Sample.Application (
    app
  ) where

import Control.Monad.Reader
import qualified System.Log.Logger as L

import Sample.Type
import Sample.Constant
import qualified Sample.Utility as U


-- |
--
app :: AppContext ()
app = do
  U.liftIOE $ print ("app called." :: String)

  lv <- _logLevelAppData <$> ask
  U.liftIOE $ L.debugM   _LOG_APP "This is a debug log message"
  U.liftIOE $ L.infoM    _LOG_APP "This is a info log message"
  U.liftIOE $ L.warningM _LOG_APP "This is a warn log message"
  U.liftIOE $ L.errorM   _LOG_APP $ "This is a error log message. " ++ show lv

  return ()
