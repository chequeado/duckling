-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.VI.Rules
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
import Duckling.Regex.Types (GroupMatch (..))
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

ruleNg :: Rule
ruleNg = Rule
  { name = "đồng"
  , pattern =
    [ regex "đ(ồng)?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly VND
  }

ruleDollar :: Rule
ruleDollar = Rule
  { name = "$"
  , pattern =
    [ regex "đô( la)?( mỹ)?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Dollar
  }

ruleVnd :: Rule
ruleVnd = Rule
  { name = "VNĐ"
  , pattern =
    [ regex "vn(Đ|D|\\$)"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly VND
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "xen|xu|cắc|penn(y|ies)"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Cent
  }

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "pounds?|pao"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Pound
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isWithoutCents
    , Predicate isNatural
    ]
  , prod = \case
      (Token AmountOfMoney fd:
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersectAndNumeral :: Rule
ruleIntersectAndNumeral = Rule
  { name = "intersect and number"
  , pattern =
    [ Predicate isWithoutCents
    , regex "và"
    , Predicate isNatural
    ]
  , prod = \case
      (Token AmountOfMoney fd:
       _:
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersectXXuxen :: Rule
ruleIntersectXXuxen = Rule
  { name = "intersect (X xu|xen)"
  , pattern =
    [ Predicate isWithoutCents
    , Predicate isCents
    ]
  , prod = \case
      (Token AmountOfMoney fd:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersectVXXuxen :: Rule
ruleIntersectVXXuxen = Rule
  { name = "intersect (và X xu|xen)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "và"
    , Predicate isCents
    ]
  , prod = \case
      (Token AmountOfMoney fd:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleDirham :: Rule
ruleDirham = Rule
  { name = "AED"
  , pattern =
    [ regex "AED\\.|dirhams?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly AED
  }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about|exactly <amount-of-money>"
  , pattern =
    [ regex "đúng|chính xác|khoảng( chừng)?|tầm|xấp xỉ|gần"
    , Predicate isMoneyWithValue
    ]
  , prod = \case
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
  { name = "between|from <numeral> to|and <amount-of-money>"
  , pattern =
    [ regex "từ|giữa|khoảng"
    , Predicate isPositive
    , regex "đến|tới|và"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
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
  { name = "từ|giữa <amount-of-money> đến|tới|và <amount-of-money>"
  , pattern =
    [ regex "từ|giữa"
    , Predicate isSimpleAmountOfMoney
    , regex "đến|tới|và"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
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
    , regex "-"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = from}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) | from < to->
         Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<amount-of-money> - <amount-of-money>"
  , pattern =
    [ Predicate isSimpleAmountOfMoney
    , regex "-"
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

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "under/less/lower/no more than <amount-of-money>"
  , pattern =
    [ regex "dưới|(bé|ít|thấp|kém|không( cao| nhiều)?) hơn|không (đến|tới|quá)|(nhiều|cao) nhất"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney . withMax to $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalMax2 :: Rule
ruleIntervalMax2 = Rule
  { name = "under <amount-of-money>"
  , pattern =
    [ regex "từ"
    , Predicate isSimpleAmountOfMoney
    , regex "trở xuống"
    ]
  , prod = \case
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
    [ regex "trên|(lớn |nhiều |cao |không (thấp|ít|kém) )?hơn|(ít|thấp) nhất"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney . withMin to $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalMin2 :: Rule
ruleIntervalMin2 = Rule
  { name = "above <amount-of-money>"
  , pattern =
    [ regex "từ"
    , Predicate isSimpleAmountOfMoney
    , regex "trở lên"
    ]
  , prod = \case
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
  , ruleCent
  , ruleDirham
  , ruleDollar
  , ruleIntersect
  , ruleIntersectAndNumeral
  , ruleIntersectVXXuxen
  , ruleIntersectXXuxen
  , ruleIntervalBetweenNumeral
  , ruleIntervalBetween
  , ruleIntervalDash
  , ruleIntervalNumeralDash
  , ruleIntervalMax
  , ruleIntervalMax2
  , ruleIntervalMin
  , ruleIntervalMin2
  , ruleNg
  , rulePounds
  , rulePrecision
  , ruleVnd
  ]
