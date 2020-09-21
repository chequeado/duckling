-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.EN.Corpus
  ( corpus
  , negativeCorpus
  , latentCorpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Resolve (Options(..))
import Duckling.Testing.Types

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, examples)
  where
    examples =
      [ "exactly dollars"
      ]

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

latentCorpus :: Corpus
latentCorpus = (testContext, testOptions {withLatent = True}, xs)
  where
    xs = concat
      [ examples (simple Unnamed 5)
                 [ "five"
                 , "5"
                 , "about 5"
                 ]
      , examples (simple Unnamed 7.2)
                 [ "7.2"
                 , "7.20000"
                 ]
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 1)
             [ "$1"
             , "one dollar"
             , "a dollar"
             ]
  , examples (simple Dollar 10)
             [ "$10"
             , "$ 10"
             , "10$"
             , "10 dollars"
             , "ten dollars"
             ]
  , examples (simple Cent 10)
             [ "10 cent"
             , "ten pennies"
             , "ten cents"
             , "10 c"
             , "10¢"
             ]
  , examples (simple Dollar 1e4)
             [ "$10K"
             , "10k$"
             , "$10,000"
             ]
  , examples (simple USD 3.14)
             [ "USD3.14"
             , "3.14US$"
             , "US$ 3.14"
             ]
  , examples (simple EUR 20)
             [ "20\x20ac"
             , "20 euros"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             , "EUR 20.0"
             , "20€"
             , "20 €ur"
             ]
  , examples (simple Pound 10)
             [ "\x00a3\&10"
             , "ten pounds"
             ]
  , examples (simple INR 20)
             [ "Rs. 20"
             , "Rs 20"
             , "20 Rupees"
             , "20Rs"
             , "Rs20"
             ]
  , examples (simple Dollar 20.43)
             [ "$20 and 43c"
             , "20 dollar 43c"
             , "20 dollars 43 cents"
             ]
  , examples (between Dollar (10, 20))
             [ "between 10 and 20 dollars"
             , "from 10 dollars to 20 dollars"
             , "around 10-20 dollars"
             , "between 10 dollars and 20 dollars"
             , "from 10 to 20 dollars"
             , "about $10-$20"
             , "10-20 dollars"
             ]
  , examples (between Dollar (1.1, 1.3))
             [ "between 1.1 and 1.3 dollars"
             , "from 1 point 1 and one point three dollars"
             ]
  , examples (under EUR 7)
             [ "under seven euros"
             , "less than 7 EUR"
             , "lower than 7€"
             , "no more than 7 euros"
             , "at most 7€"
             ]
  , examples (above Dollar 1.42)
             [ "more than 1 dollar and forty-two cents"
             , "at least $1.42"
             , "over 1.42 dollars"
             , "above a dollar and 42 cents"
             , "no less than $1.42"
             ]
   , examples (simple MNT 10)
              [ "ten tugriks"
              , "10 Tugrik"
              , "10MNT"
              ]
   , examples (simple USD 4.7e9)
              [ "US$4.7 billion"
              , "a US$4.7 billion"
              , "a US$ 4.7 billion"
              ]
  ]
