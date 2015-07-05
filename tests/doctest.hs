module Main where

-- standard
-- third party
import Test.DocTest

-- local

main :: IO ()
main = doctest [ "-i./src/"
               , "src/Database/SQLFragment/SQLFragment.hs"
               , "src/Database/SQLFragment/Operators.hs"
               , "src/Database/SQLFragment/Join.hs"
               , "src/Database/SQLFragment/Internal.hs"
               ]

