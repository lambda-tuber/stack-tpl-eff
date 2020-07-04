module Sample.Constant where

--------------------------------------------------------------------------------
-- |
--
_LOG_APP :: String
_LOG_APP = "APP"

-- |
--
_LOG_FORMAT :: String
_LOG_FORMAT = "$time [$pid($tid)] $prio $loggername - $msg"

-- |
--
_LOG_FORMAT_DATE :: String
_LOG_FORMAT_DATE = "%Y-%m-%d %H:%M:%S.%q"
