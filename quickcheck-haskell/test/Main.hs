{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import MyLib
import Test.QuickCheck
import qualified Test.Tyche as Tyche

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

prop_insertValid :: Property
prop_insertValid =
  Tyche.visualize "prop_insert_valid" $
    forAll ((,) <$> arbitrary <*> genBST (-10, 10)) $ \(x, t) ->
      label ("size:" ++ show (size t)) $
        isBST (insert x t)

prop_insertPost :: Int -> Tree -> Property
prop_insertPost x t =
  Tyche.visualize "prop_insert_post" $
    label ("size:" ++ show (size t)) $
      label ("value:" ++ show x) $
        isBST t ==>
          member x (insert x t)

return []

main :: IO Bool
main = $quickCheckAll

-- quickCheck prop_insertValid
-- Tyche.visualizeResult "prop_insertPost"
--   =<< quickCheckResult prop_insertPost
