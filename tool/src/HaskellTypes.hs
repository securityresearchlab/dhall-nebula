{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HaskellTypes (makeTypes) where

import Dhall.TH as TH
import Language.Haskell.TH

makeTypes :: Q [Dec]
makeTypes = TH.makeHaskellTypes [
    TH.SingleConstructor "TestType" "MakeTestType" "./dhall/TestType.dhall"
  , TH.SingleConstructor "BType" "MakeBType" "./dhall/BType.dhall"
  , TH.MultipleConstructors "AType" "./dhall/AType.dhall"
  ]
