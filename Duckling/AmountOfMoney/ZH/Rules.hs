-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.ZH.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types (Currency (..), AmountOfMoneyData (..))
import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (isNatural, isPositive, oneOf)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.AmountOfMoney.Types as TAmountOfMoney
import qualified Duckling.Numeral.Types as TNumeral

currencies :: HashMap Text Currency
currencies = HashMap.fromList
  [ ("aed", AED)
  , ("aud", AUD)
  , ("bgn", BGN)
  , ("brl", BRL)
  , ("byn", BYN)
  , ("cad", CAD)
  , ("¢", Cent)
  , ("c", Cent)
  , ("chf", CHF)
  , ("cny", CNY)
  , ("czk", CZK)
  , ("rmb", CNY)
  , ("yuan", CNY)
  , ("$", Dollar)
  , ("dinar", Dinar)
  , ("dinars", Dinar)
  , ("dkk", DKK)
  , ("dollar", Dollar)
  , ("dollars", Dollar)
  , ("egp", EGP)
  , ("€", EUR)
  , ("eur", EUR)
  , ("euro", EUR)
  , ("euros", EUR)
  , ("eurs", EUR)
  , ("€ur", EUR)
  , ("€uro", EUR)
  , ("€uros", EUR)
  , ("€urs", EUR)
  , ("gbp", GBP)
  , ("gel", GEL)
  , ("hkd", HKD)
  , ("hrk", HRK)
  , ("idr", IDR)
  , ("ils", ILS)
  , ("₪", ILS)
  , ("nis", ILS)
  , ("inr", INR)
  , ("iqd", IQD)
  , ("rs", INR)
  , ("rs.", INR)
  , ("rupee", INR)
  , ("rupees", INR)
  , ("jmd", JMD)
  , ("jod", JOD)
  , ("¥", JPY)
  , ("jpy", JPY)
  , ("lari", GEL)
  , ("\x20BE", GEL)
  , ("yen", JPY)
  , ("krw", KRW)
  , ("kwd", KWD)
  , ("lbp", LBP)
  , ("mad", MAD)
  , ("mnt", MNT)
  , ("myr", MYR)
  , ("rm", MYR)
  , ("₮", MNT)
  , ("tugrik", MNT)
  , ("tugriks", MNT)
  , ("nok", NOK)
  , ("nzd", NZD)
  , ("pkr", PKR)
  , ("pln", PLN)
  , ("£", Pound)
  , ("pesos", ARS)
  , ("pt", PTS)
  , ("pta", PTS)
  , ("ptas", PTS)
  , ("pts", PTS)
  , ("qar", QAR)
  , ("₽", RUB)
  , ("rial", Rial)
  , ("rials", Rial)
  , ("riyal", Riyal)
  , ("riyals", Riyal)
  , ("ron", RON)
  , ("rub", RUB)
  , ("sar", SAR)
  , ("sek", SEK)
  , ("sgd", SGD)
  , ("shekel", ILS)
  , ("shekels", ILS)
  , ("thb", THB)
  , ("ttd", TTD)
  , ("usd", USD)
  , ("us$", USD)
  , ("vnd", VND)
  , ("zar", ZAR)
  ]

ruleCurrencies :: Rule
ruleCurrencies = Rule
  { name = "currencies"
  , pattern =
    [ regex "(aed|aud|bgn|brl|byn|¢|cad|chf|cny|c|\\$|dinars?|dkk|dollars?|egp|(e|€)uro?s?|€|gbp|gel|\x20BE|hrk|idr|ils|₪|inr|iqd|jmd|jod|¥|jpy|lari|krw|kwd|lbp|mad|₮|mnt|tugriks?|myr|rm|nis|nok|nzd|£|pesos|pkr|pln|pta?s?|qar|₽|rs\\.?|riy?als?|ron|rub|rupees?|sar|sek|sgb|shekels?|thb|ttd|us(d|\\$)|vnd|yen|yuan|zar)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        c <- HashMap.lookup (Text.toLower match) currencies
        Just . Token AmountOfMoney $ currencyOnly c
      _ -> Nothing
  }

ruleCNY :: Rule
ruleCNY = Rule
  { name = "cny"
  , pattern =
    [ regex "人民币|人民幣"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly CNY
  }

ruleCNYPrefix :: Rule
ruleCNYPrefix = Rule
  { name = "cny prefix"
  , pattern =
    [ regex "人民币|人民幣"
    , Predicate isPositive
    ]
  , prod = \case
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token AmountOfMoney . withValue v $ currencyOnly CNY
      _ -> Nothing
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "分"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Cent
}

ruleDime :: Rule
ruleDime = Rule
  { name = "dime"
  , pattern =
    [ Predicate isPositive
    , regex "角|毛"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token AmountOfMoney $
        withCents (v * 10) $ currencyOnly Cent
      _ -> Nothing
  }

ruleDollar :: Rule
ruleDollar = Rule
  { name = "dollar"
  , pattern =
    [ regex "元|圆|块"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Dollar
  }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "exactly/about <amount-of-money>"
  , pattern =
    [ regex "刚好|恰好|大概"
    , Predicate isMoneyWithValue
    ]
  , prod = \case
      (_:token:_) -> Just token
      _ -> Nothing
  }

rulePrecision2 :: Rule
rulePrecision2 = Rule
  { name = "about <amount-of-money>"
  , pattern =
    [ Predicate isMoneyWithValue
    , regex "左右"
    ]
  , prod = \case
      (token:_) -> Just token
      _ -> Nothing
  }

ruleIntersectDimesAndCents :: Rule
ruleIntersectDimesAndCents = Rule
  { name = "intersect (X dimes and X cents)"
  , pattern =
    [ Predicate $ and . sequence [isSimpleAmountOfMoney, isDime]
    , Predicate $ and . sequence [isSimpleAmountOfMoney, isCent]
    ]
  , prod = \case
      (Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just d}:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just c}:
       _) ->
         Just . Token AmountOfMoney $ withCents (c + d) $ currencyOnly Cent
      _ -> Nothing
  }

ruleIntersectDollarsAndDimesCents :: Rule
ruleIntersectDollarsAndDimesCents = Rule
  { name = "intersect (X dollars and X dimes/cents)"
  , pattern =
    [ Predicate $ and . sequence [isSimpleAmountOfMoney, isWithoutCents]
    , Predicate $ and . sequence [isSimpleAmountOfMoney, isCents]
    ]
  , prod = \case
      (Token AmountOfMoney fd:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect (implicit 0 delimited cents)"
  , pattern =
    [ Predicate $ and . sequence [isSimpleAmountOfMoney, isWithoutCents]
    , regex "0|零|〇"
    , oneOf [1..9]
    ]
  , prod = \case
      (Token AmountOfMoney fd:_:
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersect2 :: Rule
ruleIntersect2 = Rule
  { name = "intersect (implicit unitless cents)"
  , pattern =
    [ Predicate $ and . sequence [isSimpleAmountOfMoney, isDime]
    , Predicate isNatural
    ]
  , prod = \case
      (Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just v}:
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just . Token AmountOfMoney $ withCents (v + c) $ currencyOnly Cent
      _ -> Nothing
  }

ruleIntersect3 :: Rule
ruleIntersect3 = Rule
  { name = "intersect (implicit unitless dimes)"
  , pattern =
    [ Predicate $ and . sequence [isSimpleAmountOfMoney, isWithoutCents]
    , Predicate isNatural
    ]
  , prod = \case
      (Token AmountOfMoney fd:
       Token Numeral NumeralData{TNumeral.value = d}:
       _) -> Just . Token AmountOfMoney $ withCents (d * 10) fd
      _ -> Nothing
  }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash = Rule
  { name = "<numeral> - <amount-of-money>"
  , pattern =
    [ Predicate isPositive
    , regex "-|~|到"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = from}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) | from < to ->
         Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<amount-of-money> - <amount-of-money>"
  , pattern =
    [ Predicate isSimpleAmountOfMoney
    , regex "-|~|到"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just from,
                  TAmountOfMoney.currency = c1}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c2}:
       _) | from < to && c1 == c2 ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c1
      _ -> Nothing
  }

ruleIntervalBound :: Rule
ruleIntervalBound = Rule
  { name = "under/less/lower/no more than <amount-of-money> (最多|至少|最少)"
  , pattern =
    [ regex "(最多|至少|最少)"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) -> case match of
        "最多" -> Just . Token AmountOfMoney . withMax to $ currencyOnly c
        "最少" -> Just . Token AmountOfMoney . withMin to $ currencyOnly c
        "至少" -> Just . Token AmountOfMoney . withMin to $ currencyOnly c
        _ -> Nothing
      _ -> Nothing
  }

ruleIntervalBound2 :: Rule
ruleIntervalBound2 = Rule
  { name = "under/less/lower/no more than <amount-of-money> (以下|以上)"
  , pattern =
    [ Predicate isSimpleAmountOfMoney
    , regex "(以下|以上)"
    ]
  , prod = \case
      (Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case match of
        "以下" -> Just . Token AmountOfMoney . withMax to $ currencyOnly c
        "以上" -> Just . Token AmountOfMoney . withMin to $ currencyOnly c
        _ -> Nothing
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleCurrencies
  , ruleCent
  , ruleCNY
  , ruleCNYPrefix
  , ruleDime
  , ruleDollar
  , ruleIntersect
  , ruleIntersect2
  , ruleIntersect3
  , ruleIntersectDimesAndCents
  , ruleIntersectDollarsAndDimesCents
  , ruleIntervalDash
  , ruleIntervalNumeralDash
  , ruleIntervalBound
  , ruleIntervalBound2
  , rulePrecision
  , rulePrecision2
  ]
