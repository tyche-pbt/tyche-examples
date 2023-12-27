{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.Aeson
  ( Options (fieldLabelModifier, sumEncoding),
    SumEncoding (UntaggedValue),
    ToJSON (toEncoding),
    defaultOptions,
    encode,
    genericToEncoding,
  )
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import MyLib
import System.Directory (createDirectoryIfMissing)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Testable,
    choose,
    forAll,
    frequency,
    label,
    quickCheckResult,
    (==>),
  )
import Test.QuickCheck.Property (Callback (PostTest), CallbackKind (NotCounterexample), Result (labels, ok, reason, testCase), callback)
import Text.Read (readMaybe)

data FeatureData
  = IntData Int
  | StringData String
  deriving (Generic, Show)

instance ToJSON FeatureData where
  toEncoding =
    genericToEncoding
      ( defaultOptions
          { fieldLabelModifier = drop 1,
            sumEncoding = UntaggedValue
          }
      )

data InfoLine = InfoLine
  { _info_type :: String,
    _info_run_start :: Double,
    _info_property :: String,
    _info_title :: String,
    _info_content :: String
  }
  deriving (Generic, Show)

instance ToJSON InfoLine where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = drop 6})

data TestCaseLine = TestCaseLine
  { _tc_type :: String,
    _tc_run_start :: Double,
    _tc_property :: String,
    _tc_status :: String,
    _tc_status_reason :: String,
    _tc_representation :: String,
    _tc_features :: Map String FeatureData,
    _tc_coverage :: Maybe ()
  }
  deriving (Generic, Show)

instance ToJSON TestCaseLine where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = drop 4})

quickCheckVis :: (Testable prop) => String -> prop -> IO ()
quickCheckVis propName prop = do
  runStart <- fromRational . toRational <$> getPOSIXTime
  res <- quickCheckResult (newProp runStart prop)
  writeLine $
    InfoLine
      { _info_type = "info",
        _info_run_start = runStart,
        _info_property = propName,
        _info_title = "QuickCheck Result",
        _info_content = show res
      }
  where
    newProp runStart p = do
      callback
        ( PostTest
            NotCounterexample
            ( \_ res ->
                writeLine $
                  TestCaseLine
                    { _tc_type = "test_case",
                      _tc_run_start = runStart,
                      _tc_property = propName,
                      _tc_status = case ok res of
                        Nothing -> "gave_up"
                        Just True -> "passed"
                        Just False -> "failed",
                      _tc_status_reason = reason res,
                      _tc_representation = case testCase res of [x] -> x; xs -> show xs,
                      _tc_features = M.fromList (map parseLabel (labels res)),
                      _tc_coverage = Nothing
                    }
            )
        )
        p

    writeLine :: (ToJSON a) => a -> IO ()
    writeLine x = do
      createDirectoryIfMissing True ".quickcheck/observations"
      BSL.appendFile (".quickcheck/observations/" ++ propName ++ ".jsonl") . flip BSL.snoc '\n' . encode $ x

    parseLabel :: String -> (String, FeatureData)
    parseLabel l
      | ':' `elem` l =
          (takeWhile (/= ':') l, parseData (drop 1 (dropWhile (/= ':') l)))
      where
        parseData s = case readMaybe s of
          Just n -> IntData n
          Nothing -> StringData s
    parseLabel l = (l, StringData "")

instance Arbitrary Tree where
  arbitrary = aux (3 :: Int)
    where
      aux 0 = return Leaf
      aux n =
        frequency
          [ (1, return Leaf),
            ( n,
              do
                x <- arbitrary
                l <- aux (n - 1)
                r <- aux (n - 1)
                return (Node l x r)
            )
          ]

genBST :: (Int, Int) -> Gen Tree
genBST (lo, hi) | lo > hi = return Leaf
genBST (lo, hi) =
  frequency
    [ (1, return Leaf),
      ( 1,
        do
          x <- choose (lo, hi)
          l <- genBST (lo, x - 1)
          r <- genBST (x + 1, hi)
          return (Node l x r)
      )
    ]

main :: IO ()
main = do
  quickCheckVis "prop_insert_valid" $ forAll ((,) <$> arbitrary <*> genBST (-10, 10)) $ \(x, t) ->
    label ("size:" ++ show (size t)) $
      isBST (insert x t)
  quickCheckVis "prop_insert_post" $ \(x, t) ->
    label ("value:" ++ show x) $
      isBST t ==>
        member x (insert x t)
