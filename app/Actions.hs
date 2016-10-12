{-# LANGUAGE OverloadedStrings #-}

module Actions
  ( FromValue(..)
  , ToValue(..)
  , Value(..)
  ) where

import           Data.Aeson (Value(..))

class FromValue a where
  fromValue :: Value -> a

class ToValue a where
  toValue :: a -> Value

