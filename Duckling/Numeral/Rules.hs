-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.Rules
  ( rules
  ) where

import Prelude

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "integer (numeric)"
  , pattern =
    [ regex "(\\d{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        toInteger <$> parseInt match >>= integer
      _ -> Nothing
  }

ruleFractions :: Rule
ruleFractions = Rule
  { name = "fractional number"
  , pattern =
    [ regex "(\\d+)/(\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (numerator:denominator:_)):_) -> do
        n <- parseDecimal False numerator
        d <- parseDecimal False denominator
        divide n d >>= notOkForAnyTime
      _ -> Nothing
  }

ruleFractions2 :: Rule
ruleFractions2 = Rule
  { name = "<numeral> %"
  , pattern =
    [ dimension Numeral
    , regex "%"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = number}:
       _:
       _) -> do
        n <- double number
        d <- integer 100
        divide n d >>= notOkForAnyTime
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleIntegerNumeric
  , ruleFractions
  , ruleFractions2
  ]
