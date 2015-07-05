{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators #-}
module Database.SQLFragment.Internal where
-- standard
import qualified Data.Map as Map
import Data.Maybe(fromMaybe)
import Data.List(intercalate)
-- third-party
import Data.List.Split (splitOn)
-- local
import Database.SQLFragment.SQLFragment


-- * Construction
--------------------------------------------------
-- Construction
--------------------------------------------------
-- * Manipulation
--------------------------------------------------
-- Manipulation
--------------------------------------------------

getSection :: Section -> SQLFragment e p -> [String]
getSection section q = fromMaybe [] ( Map.lookup section (clauses q))

setSection :: Section -> [String] -> SQLFragment e p -> SQLFragment e p

setSection section values q = SQLFragment clauses' where
    clauses' = Map.update (toMaybe values) section (clauses q)
    toMaybe [] _ = Nothing
    toMaybe values _ = Just values

clearSection section q = setSection section [] q

-- | Extract a section and create a new fragment
pickSection :: Section -> Section -> SQLFragment e p -> SQLFragment '[] '[]

pickSection src dest q = SQLFragment $ Map.singleton dest (getSection src q)

--------------------------------------------------
-- * Format
--------------------------------------------------
-- | Core function to implement the !%! operator.
--
-- >>> "$1 $2"  `format` "a"
-- "a $1"
-- >>> "$1 $2" `format` "a" `format` "b"
-- "a b"
--
format :: String -> String -> String
format fmt s = foldl (flip ($)) fmt (zipWith replace placeholders values) where
    placeholders = ["$"++show i | i <- [1..9]]
    values = s:placeholders
    replace old new = intercalate new . splitOn old
