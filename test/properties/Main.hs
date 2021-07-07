{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import qualified CountEq as CE
import Data.Interieur.ByteArray (countEq, findFirstGt, findLastEq)
import qualified FindFirstGt as FFG
import qualified FindLastEq as FLE
import Test.QuickCheck (Property, forAllShrink, (===))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (QuickCheckTests), testProperty)

main :: IO ()
main =
  defaultMain . localOption (QuickCheckTests 100000) . testGroup "Properties" $
    [ testGroup
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
          ]
    ]

-- Helpers

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
