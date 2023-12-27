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
import Test.QuickCheck.Property (Callback (PostTest), CallbackKind (NotCounterexample), Result (ok), callback)

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

data TestCaseLine = TestCaseLine
  { _type :: String,
    _run_start :: Double,
    _property :: String,
    _status :: String,
    _status_reason :: String,
    _representation :: String,
    _features :: Map String FeatureData,
    _coverage :: Maybe ()
  }
  deriving (Generic, Show)

instance ToJSON TestCaseLine where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = drop 1})

class (Show a) => Reportable a where
  extractOrdinal :: a -> Map String Int
  extractNominal :: a -> Map String String

quickCheckVis :: (Reportable a, Arbitrary a) => String -> Gen a -> (a -> Property) -> IO ()
quickCheckVis name g prop = do
  createDirectoryIfMissing True ".quickcheck/observations"
  runStart <- fromRational . toRational <$> getPOSIXTime
  quickCheck (forAll g $ newProp runStart prop)
  where
    newProp runStart p a = do
      callback
        ( PostTest
            NotCounterexample
            ( \_ res ->
                BSL.appendFile (".quickcheck/observations/" ++ name ++ ".jsonl") . flip BSL.snoc '\n' . encode $
                  TestCaseLine
                    { _type = "test_case",
                      _run_start = runStart,
                      _property = name,
                      _status = case ok res of
                        Nothing -> "gave_up"
                        Just True -> "passed"
                        Just False -> "failed",
                      _status_reason = "",
                      _representation = show a,
                      _features = M.union (fmap IntData (extractOrdinal a)) (fmap StringData (extractNominal a)),
                      _coverage = Nothing
                    }
            )
        )
        (p a)

instance Reportable Int where
  extractOrdinal _ = M.empty
  extractNominal _ = M.empty

instance Reportable Tree where
  extractOrdinal t = M.fromList [("size", size t)]
  extractNominal t = M.fromList [("isBST", show (isBST t))]

instance (Reportable a, Reportable b) => Reportable (a, b) where
  extractOrdinal (a, b) = M.union (extractOrdinal a) (extractOrdinal b)
  extractNominal (a, b) = M.union (extractNominal a) (extractNominal b)

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
  quickCheckVis "prop_insert_valid" arbitrary $ \(x, t) -> isBST t ==> isBST (insert x t)
  quickCheckVis "prop_insert_valid_ok" ((,) <$> arbitrary <*> genBST (-10, 10)) $ \(x, t) -> isBST t ==> isBST (insert x t)
