{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import qualified CountEq as CE
import Data.Diablerie.ByteArray
  ( countEq,
    findFirstGt,
    findFirstMatch,
    findFirstNe,
    findLastEq,
  )
import qualified FindFirstGt as FFG
import qualified FindFirstMatch as FFM
import qualified FindFirstNe as FFN
import qualified FindLastEq as FLE
import Test.QuickCheck (Property, forAllShrink, (.&&.), (=/=), (===))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (QuickCheckTests), testProperty)

main :: IO ()
main =
  defaultMain . localOption (QuickCheckTests 100000) . testGroup "Properties" $
    [ testGroup
        "findFirstNe"
        [ testProperty "Exclusion" ffnExclusionProp,
          testProperty "Inclusion" ffnInclusionProp
        ],
      testGroup
        "findLastEq"
        [ testProperty "Exclusion" fleExclusionProp,
          testProperty "Inclusion" fleInclusionProp
        ],
      testGroup
        "findFirstGt"
        [ testProperty "Exclusion" ffgExclusionProp,
          testProperty "Inclusion" ffgInclusionProp
        ],
      localOption (QuickCheckTests 10000)
        . testGroup
          "countEq"
        $ [ testProperty "Counting" ceCountingProp
          ],
      localOption (QuickCheckTests 50000)
        . testGroup
          "findFirstMatch"
        $ [ testProperty "Exclusion" ffmExclusionProp,
            testProperty "Inclusion" ffmInclusionProp,
            testProperty "Prefix" ffmPrefixProp
          ]
    ]

-- Helpers

ffnExclusionProp :: Property
ffnExclusionProp = forAllShrink arbitrary shrink go
  where
    go :: FFN.Exclusion -> Property
    go (FFN.Exclusion ba w8) =
      findFirstNe ba w8 === Nothing

ffnInclusionProp :: Property
ffnInclusionProp = forAllShrink arbitrary shrink go
  where
    go :: FFN.Inclusion -> Property
    go (FFN.Inclusion ba w8 ix) =
      findFirstNe ba w8 === Just ix

ffmPrefixProp :: Property
ffmPrefixProp = forAllShrink arbitrary shrink go
  where
    go :: FFM.Prefix -> Property
    go (FFM.Prefix needle haystack prefix) =
      (findFirstMatch prefix haystack =/= Nothing)
        .&&. ( findFirstMatch prefix haystack
                 `compare` findFirstMatch needle haystack =/= GT
             )

ffmInclusionProp :: Property
ffmInclusionProp = forAllShrink arbitrary shrink go
  where
    go :: FFM.Inclusion -> Property
    go (FFM.Inclusion needle haystack ix) =
      findFirstMatch needle haystack === Just ix

ffmExclusionProp :: Property
ffmExclusionProp = forAllShrink arbitrary shrink go
  where
    go :: FFM.Exclusion -> Property
    go (FFM.Exclusion needle haystack) =
      findFirstMatch needle haystack === Nothing

ceCountingProp :: Property
ceCountingProp = forAllShrink arbitrary shrink go
  where
    go :: CE.CountEq -> Property
    go (CE.CountEq ba w8 count) = countEq ba w8 === count

ffgInclusionProp :: Property
ffgInclusionProp = forAllShrink arbitrary shrink go
  where
    go :: FFG.Inclusion -> Property
    go (FFG.Inclusion ba w8 ix) = findFirstGt ba w8 === Just ix

ffgExclusionProp :: Property
ffgExclusionProp = forAllShrink arbitrary shrink go
  where
    go :: FFG.Exclusion -> Property
    go (FFG.Exclusion ba w8) = findFirstGt ba w8 === Nothing

fleInclusionProp :: Property
fleInclusionProp = forAllShrink arbitrary shrink go
  where
    go :: FLE.Inclusion -> Property
    go (FLE.Inclusion ba w8 ix) = findLastEq ba w8 === Just ix

fleExclusionProp :: Property
fleExclusionProp = forAllShrink arbitrary shrink go
  where
    go :: FLE.Exclusion -> Property
    go (FLE.Exclusion ba w8) = findLastEq ba w8 === Nothing
