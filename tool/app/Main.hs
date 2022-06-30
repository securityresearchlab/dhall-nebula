{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- module Main where

import Lib
import HaskellTypes
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO
import Control.Monad

-- main :: IO ()
-- main = do
--   _ <- unQ makeTypes
--   let x = $makeTypes

$makeTypes

deriving instance Show TestType
deriving instance Show AType
deriving instance Show BType
