{-# LANGUAGE TemplateHaskell #-}

module Sample.Type where

import Data.Data
import Data.Default
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Extensible as EX
import qualified Data.Extensible.Effect as EX
import qualified Data.Extensible.Effect.Default as EX
import qualified System.Log.Logger as L
import qualified Data.Text as T
import qualified Text.Read as R

--------------------------------------------------------------------------------
-- |
--
instance FromJSON L.Priority where
  parseJSON (String v) = case R.readEither (T.unpack v) of
    Right l -> pure l
    Left er -> error $ "invalid loglevel. <" ++ T.unpack v ++ "> " ++ er
  parseJSON o = error $ "json parse error. Priority:" ++ show o

instance ToJSON L.Priority where
  toJSON (L.DEBUG)     = String $ T.pack "DEBUG"
  toJSON (L.INFO)      = String $ T.pack "INFO"
  toJSON (L.NOTICE)    = String $ T.pack "NOTICE"
  toJSON (L.WARNING)   = String $ T.pack "WARNING"
  toJSON (L.ERROR)     = String $ T.pack "ERROR"
  toJSON (L.CRITICAL)  = String $ T.pack "CRITICAL"
  toJSON (L.ALERT)     = String $ T.pack "ALERT"
  toJSON (L.EMERGENCY) = String $ T.pack "EMERGENCY"


--------------------------------------------------------------------------------
-- |
--
data ArgData = ArgData {
    _yamlArgData :: Maybe FilePath
  } deriving (Data, Typeable, Show, Read, Eq)

makeLenses ''ArgData
$(deriveJSON
  defaultOptions {
      fieldLabelModifier = tail . reverse . drop (length "ArgData") . reverse
    }
  ''ArgData)

instance Default ArgData where
  def = ArgData {
        _yamlArgData = Nothing
      }


--------------------------------------------------------------------------------
-- |
--
data ConfigData = ConfigData {
    _logFileConfigData  :: FilePath
  , _logLevelConfigData :: L.Priority
  } deriving (Typeable, Show, Read, Eq)

makeLenses ''ConfigData
$(deriveJSON
  defaultOptions {
      fieldLabelModifier = tail . reverse . drop (length "ConfigData") . reverse
    }
  ''ConfigData)

instance Default ConfigData where
  def = ConfigData {
        _logFileConfigData  = "sample.log"
      , _logLevelConfigData = L.WARNING
      }


--------------------------------------------------------------------------------
-- |
--
data AppData = AppData {
    _logLevelAppData :: L.Priority
  }

makeLenses ''AppData

instance Default AppData where
  def = AppData {
        _logLevelAppData = L.WARNING
      }

--------------------------------------------------------------------------------
-- |
--
type ErrorData = String
type AppContext r = EX.Eff '[
                     EX.ReaderDef AppData
                   , EX.EitherDef ErrorData
                   , "IO" EX.>: IO
                   ] r

