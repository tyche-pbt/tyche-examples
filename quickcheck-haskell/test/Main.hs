{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.Aeson
  ( Options (fieldLabelModifier),
    ToJSON (toEncoding),
    defaultOptions,
    encode,
    genericToEncoding,
  )
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import GHC.IORef (IORef)
import MyLib
import Network.WebSockets (runClient, sendTextData)
import Test.QuickCheck

data SampleInfo = SampleInfo
  { _item :: String,
    _features :: Map String Int,
    _bucketings :: Map String String
  }
  deriving (Generic, Show)

instance ToJSON SampleInfo where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = drop 1})

data CoverageItem = CoverageItem
  { _covered :: [Int],
    _missedLines :: [Int]
  }
  deriving (Generic, Show)

instance ToJSON CoverageItem where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = drop 1})

data TestInfo
  = PropertyPassed
      { _outcome :: String,
        _samples :: [SampleInfo],
        _coverage :: Map String CoverageItem
      }
  | PropertyFailed
      { _message :: String,
        _outcome :: String
      }
  deriving (Generic, Show)

instance ToJSON TestInfo where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = drop 1})

newtype Report = Report {_properties :: Map String TestInfo}
  deriving (Generic, Show)

instance ToJSON Report where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = drop 1})

data Request = Request {_type :: String, _report :: Report}
  deriving (Generic, Show)

instance ToJSON Request where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = drop 1})

class (Show a) => Reportable a where
  extractFeatures :: a -> Map String Int
  extractBucketings :: a -> Map String String

visualize :: (Reportable a) => [a] -> TestInfo
visualize items =
  PropertyPassed
    { _outcome = "propertyPassed",
      _samples = map toSampleInfo items,
      _coverage = M.empty
    }
  where
    toSampleInfo i =
      SampleInfo
        { _item = show i,
          _features = extractFeatures i,
          _bucketings = extractBucketings i
        }

quickCheckVis :: (Reportable a, Arbitrary a) => String -> Gen a -> (a -> Property) -> IO ()
quickCheckVis name g prop = do
  examples <- newIORef []
  res <- quickCheckResult (forAll g $ newProp examples prop)
  report <- case res of
    Success {} -> do
      exs <- readIORef examples
      return $ Report (M.fromList [(name, visualize exs)])
    e -> return $ Report (M.fromList [(name, PropertyFailed (show e) "propertyFailed")])
  let request = Request {_type = "success", _report = report}
  runClient "localhost" 8181 "/" (\conn -> sendTextData conn (encode request))
  where
    newProp :: (Reportable a) => IORef [a] -> (a -> Property) -> a -> Property
    newProp examples p a = ioProperty (modifyIORef examples (a :) >> return (p a))

instance Reportable Int where
  extractFeatures _ = M.empty
  extractBucketings _ = M.empty

instance Reportable Tree where
  extractFeatures t = M.fromList [("size", size t)]
  extractBucketings t = M.fromList [("isBST", show (isBST t))]

instance (Reportable a, Reportable b) => Reportable (a, b) where
  extractFeatures (a, b) = M.union (extractFeatures a) (extractFeatures b)
  extractBucketings (a, b) = M.union (extractBucketings a) (extractBucketings b)

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
  quickCheckVis "prop_insert_valid'" ((,) <$> arbitrary <*> genBST (-10, 10)) $ \(x, t) -> isBST t ==> isBST (insert x t)
