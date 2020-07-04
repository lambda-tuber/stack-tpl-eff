{-# LANGUAGE OverloadedStrings #-}

module Sample.Utility where

import qualified Control.Exception.Safe as E
import Control.Monad.IO.Class
import Control.Monad.Except
import qualified Data.Extensible.Effect as EX
import qualified Data.Extensible.Effect.Default as EX
import qualified System.Log.Logger as L
import qualified System.Log.Formatter as L
import qualified System.Log.Handler as LH
import qualified System.Log.Handler.Simple as LHS
import System.IO

import Sample.Type
import Sample.Constant


-- |
--
runApp :: AppData -> AppContext a -> IO (Either ErrorData a)
runApp appDat =
  EX.retractEff
  . EX.runEitherDef
  . flip EX.runReaderDef appDat


-- |
--
liftIOE :: IO a -> AppContext a
liftIOE f = liftIO (go f) >>= liftEither
  where
    go :: IO b -> IO (Either String b)
    go f = E.catchAny (Right <$> f) errHdl

    errHdl :: E.SomeException -> IO (Either String a)
    errHdl = return . Left . show


-- |
--
setupLogger :: L.Priority -> FilePath -> IO ()
setupLogger lv path = do

  logStream <- openFile path AppendMode
  hSetEncoding logStream utf8

  logH <- LHS.streamHandler logStream lv

  let logHandle  = logH {LHS.closeFunc = hClose}
      logFormat  = L.tfLogFormatter _LOG_FORMAT_DATE _LOG_FORMAT
      logHandler = LH.setFormatter logHandle logFormat

  L.updateGlobalLogger L.rootLoggerName $ L.setHandlers ([] :: [LHS.GenericHandler Handle])
  L.updateGlobalLogger _LOG_APP $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_APP $ L.setLevel lv


