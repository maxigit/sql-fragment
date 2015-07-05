{-# LANGUAGE TemplateHaskell, DataKinds #-}
module Main  where

import System.Exit (exitFailure)
import Control.Monad (unless)

import Test.QuickCheck
import Test.QuickCheck.All

{-
import Database.SQLFragment
import Database.SQLFragment.Examples

a = fromList [] :: SQLFragment '[]
b = fromList [""] :: SQLFragment '[]
fromList :: [String] -> SQLFragment a
fromList columns = fEmpty { fColumns = columns
                      , fTables = map ("T"++) columns
                      , fWheres = map ("W"++) columns
                      , fGroups = map ("G"++) columns
                      , fHavings = map ("H"++) columns
                      , fOrders = map ("O"++) columns
                      }
instance Arbitrary (SQLFragment a) where
    arbitrary = do
        columns <- arbitrary
        return (fromList columns)
    shrink q = map (fromList.shrink) (fColumns q)
    {-
    shrink q = [ q {fColumns= tail (fColumns q)}
               , q {fTables= tail (fTables q)}
               , q {fWheres= tail (fWheres q)}
               , q {fGroups= tail (fGroups q)}
               , q {fHavings= tail (fWheres q)}
               , q {fOrders= tail (fWheres q)}
               ]
    shrink q = [ fEmpty {fColumns= (fColumns q)}
               , fEmpty {fTables= (fTables q)}
               , fEmpty {fWheres= (fWheres q)}
               , fEmpty {fGroups= (fGroups q)}
               , fEmpty {fHavings= (fWheres q)}
               , fEmpty {fOrders= (fWheres q)}
               ]
    -}
                

infix 0 =?,?=? 
(=?) :: String -> SQLFragment a -> Bool
s =? q = s == toSelectQuery q 

(?=?) :: SQLFragment a -> SQLFragment b -> Bool
q ?=? q' =  toSelectQuery q == toSelectQuery q'

associativy :: SQLFragment '[] -> SQLFragment '[] -> SQLFragment '[] -> Bool
associativy a b c = a !&! (b !&! c) ?=? (a !&! b) !&! c
prop_associativity = \(a, b, c) -> associativy a b c

order_composition :: SQLFragment '[] -> SQLFragment '[] -> SQLFragment '[] -> Bool
order_composition a b c = a !^! (b !&! c) ?=? (a !^! b) !^! c
prop_order_composition = \(a, b, c) -> order_composition a b c

distributivy  a b c d =
    fColumns ((a !&! b) !%! (c !&! d))
                        == fColumns ((a !%! c) !&! (c !%! d))

prop_distributivy = \(a, b,c, d) -> distributivy
    (column a)
    (column b)
    (column c)
    (column d)

-}
runTests = $(quickCheckAll)

-- main :: IO ()
main  = do
    r <- runTests
    unless r $ exitFailure
