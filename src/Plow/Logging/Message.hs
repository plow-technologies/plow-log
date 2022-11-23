{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Plow.Logging.Message
  ( LogLevel (..),
    LogMessage (..),
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withText)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Plow.Logging (HasEnumerableConstructors)

-- | Generic log levels
data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther Text
  deriving (Eq, Ord, Generic, HasEnumerableConstructors, ToJSON)

instance Show LogLevel where
  show LevelDebug = "DEBUG"
  show LevelInfo = "INFO"
  show LevelWarn = "WARN"
  show LevelError = "ERROR"
  show (LevelOther l) = Text.unpack l

instance FromJSON LogLevel where
  parseJSON = withText "LogLevel" $ \case
    "DEBUG" -> pure LevelDebug
    "INFO" -> pure LevelInfo
    "WARN" -> pure LevelWarn
    "ERROR" -> pure LevelError
    other -> pure $ LevelOther other

-- | A log message that allows for a log level to be specified or for the log
-- to be ignored.
data LogMessage a
  = Stdout LogLevel a
  | Ignore
  deriving (Show, Generic, HasEnumerableConstructors)

-- | Concatenates two log messages with a newline, keeping the log level of that has the higher severity
instance (Semigroup String) => Semigroup (LogMessage String) where
  a <> b = case (a, b) of
    (Stdout logLevelA a', Stdout logLevelB b') -> Stdout (max logLevelA logLevelB) (a' <> "\n" <> b')
    (Stdout logLevelA a', Ignore) -> Stdout logLevelA a'
    (Ignore, Stdout logLevelB b') -> Stdout logLevelB b'
    (Ignore, Ignore) -> Ignore
