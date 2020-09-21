-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.NL.Rules
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
import Duckling.AmountOfMoney.Types (Currency(..), AmountOfMoneyData (..))
import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (isNatural, isPositive)
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

ruleUnitAmount :: Rule
ruleUnitAmount = Rule
  { name = "<unit> <amount>"
  , pattern =
    [ Predicate isCurrencyOnly
    , Predicate isPositive
    ]
  , prod = \case
      (Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.currency = c}:
       Token Numeral NumeralData{TNumeral.value = v}:
       _) -> Just . Token AmountOfMoney . withValue v $ currencyOnly c
      _ -> Nothing
  }

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "pou?nds?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Pound
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "cents?|penn(y|ies)|pence|sens?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Cent
  }

ruleBucks :: Rule
ruleBucks = Rule
  { name = "bucks"
  , pattern =
    [ regex "bucks?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Unnamed
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "en"
    , Predicate isCents
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isWithoutCents
    , Predicate isNatural
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersectAndNumeral :: Rule
ruleIntersectAndNumeral = Rule
  { name = "intersect (and number)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "en"
    , Predicate isNatural
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersectXCents :: Rule
ruleIntersectXCents = Rule
  { name = "intersect (X cents)"
  , pattern =
    [ Predicate isWithoutCents
    , Predicate isCents
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about|exactly <amount-of-money>"
  , pattern =
    [ regex "precies|ongeveer|over|dicht|bijna|in de buurt|rond de"
    , Predicate isMoneyWithValue
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
  { name = "between|from <numeral> to|and <amount-of-money>"
  , pattern =
    [ regex "tussen|vanaf|van"
    , Predicate isPositive
    , regex "tot|en"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Numeral NumeralData{TNumeral.value = from}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) | from < to ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between|from <amount-of-money> to|and <amount-of-money>"
  , pattern =
    [ regex "tussen|vanaf|van"
    , Predicate isSimpleAmountOfMoney
    , regex "tot|en"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just from,
                  TAmountOfMoney.currency = c1}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c2}:
       _) | from < to && c1 == c2 ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c1
      _ -> Nothing
  }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash = Rule
  { name = "<numeral> - <amount-of-money>"
  , pattern =
    [ Predicate isNatural
    , regex "\\-"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
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
    , regex "\\-"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just from,
                  TAmountOfMoney.currency = c1}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c2}:
       _) | from < to && c1 == c2 ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c1
      _ -> Nothing
  }

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "under/less/lower/no more than <amount-of-money>"
  , pattern =
    [ regex "(minder|lager|niet? meer) dan"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney . withMax to $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "over/above/at least/more than <amount-of-money>"
  , pattern =
    [ regex "boven de|minstens|meer dan"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney . withMin to $ currencyOnly c
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleCurrencies
  , ruleUnitAmount
  , ruleBucks
  , ruleCent
  , ruleIntersect
  , ruleIntersectAndNumeral
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , ruleIntervalBetweenNumeral
  , ruleIntervalBetween
  , ruleIntervalMax
  , ruleIntervalMin
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , rulePounds
  , rulePrecision
  ]
