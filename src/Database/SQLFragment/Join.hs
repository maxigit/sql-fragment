{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{- | 'Join' are at the heart of the *autojoin* mechanism.
 - Instead of specifying the joins between table whilst building the query.
 - all the possible joins are describe in separate joins.
 - This joins is then used to generate the minimal
 - set of join needed to *cover* the tables involved by a query.
 - The main advantage of this approach is joins don't need
 - to be rewritten for each query but can be only written once
 - as a list of join. The obvious joins would be the one 
 - generated by all the foreign keys. However, it is recommended
 - to avoid muliple path by breaking full joins into small subjoins which can be combined together.
 - For some examples see the '!--!' operator.
 - Note that join are oriented, ie they only work in one direction.
 - Symmetrical joins need to be added twice in a joins (in both direction).
 -}

module Database.SQLFragment.Join
( autoJoin
, (!->!)
, (!*->!)
, (!-*>!)
, (!<->!)
, (!<-*>!)
, (!<-*->!)
, (!<*>!)
) where
-- standard libraries
import Control.Monad (sequence)
import Data.List (nub, intercalate)
import Data.Maybe (mapMaybe)
import Data.Functor((<$>))
-- third party 
import Control.Monad.Writer
-- local modules
import Database.SQLFragment.SQLFragment
import Database.SQLFragment.Operators
import Database.SQLFragment.Internal

a = "a.x" :: SQLFragment '[] '[]
b = "b.x"  :: SQLFragment '[] '[]
c = "c.x" :: SQLFragment '[] '[]
d = "d.x" :: SQLFragment '[] '[]
g = a !->! b ++ b !->! c
h = a !->! b ++ b !->! d  ++ a !<->! c ++ c !->! d
-- |  Computes the mimimum set of join needed to cover the tables from an 'SQLFragment' .
--
-- >>> :set -XDataKinds -XOverloadedStrings
-- >>> let a = "a.x" :: SQLFragment '[] '[]
-- >>> let b = "b.x"  :: SQLFragment '[] '[]
-- >>> let c = "c.x" :: SQLFragment '[] '[]
-- >>> let g = a !->! b ++ b !->! c
-- >>> either toSelectQuery toSelectQuery $ autoJoin (a !&! b) g
-- "SELECT a.x, b.x\nFROM a JOIN b ON ((a.x)=(b.x))"
-- >>> either toSelectQuery toSelectQuery $ autoJoin (b !&! c) g
-- "SELECT b.x, c.x\nFROM b JOIN c ON ((b.x)=(c.x))"
-- >>> either toSelectQuery toSelectQuery $ autoJoin (a !&!c) g
-- "SELECT a.x, c.x\nFROM a JOIN b ON ((a.x)=(b.x)) JOIN c ON ((b.x)=(c.x))"
autoJoin :: SQLFragment e p -> [Join] -> Either (SQLFragment e p) (SQLFragment e p) 
autoJoin q js = let
    tables = getSection FROM q
    path = cover tables js
    to_str join  = do
            clause <- joinClause join
            return$  to_sql (joinType join) ++ " " ++ (destination join) ++ " ON " ++ clause
    to_sql INNER = "JOIN"
    to_sql OUTER = ","
    to_sql t = show t ++ " JOIN"
    in case path of
        (Just ps) -> Right $ setSection FROM [intercalate " " $  (head tables) : mapMaybe to_str ps] q
        Nothing  -> Left q

type Graph = [Join]

{-
-- | lookup for an edge starting by the given point
-- and return the joins without it
--
-- >>> findJoin "a" [Join "a" "b" "a->b", Join "b" "c" "b->c"]
-- Just (Join {source = "a", destination = "b", joinClause = "a->b"},[Join {source = "b", destination = "c", joinClause = "b->c"}])
-- >>> findJoin "c" [Join "a" "b" "a->b", Join "b" "c" "b->c"]
-- Nothing
findJoin :: String
         -> [Join] 
         -> Maybe (Join , [Join]) -- found join , other joins
findJoin a js = findJoin' a [] js

-- | Similar to 'findJoin' but keep track of the tried element
-- We probably could have use a zipper here.
findJoin' :: String
          -> [Join]
          -> [Join]
          -> Maybe (Join, [Join])
findJoin' _ _ [] = Nothing
findJoin' a tried (r:rs) 
    | a === (source r) = Just $ (r, tried ++ rs)
    | otherwise = findJoin' a (tried ++ [r]) rs

-}

-- | Find a list of joins connecting source to destination
path :: String -- ^ source
     -> String -- ^ destination
     -> [Join] -> Maybe [Join]
path a b g = tryJoin a b [] g where
    
-- | Find a list of joins connecting source to destination
tryJoin :: String -- source
        -> String -- destination 
        -> [Join] -- tried
        -> [Join] -- to try
        -> Maybe [Join]

-- | Try all pathVia , source -> * -> destination
-- for each element in totry and get the shortest path.
-- tried totry acts as a zipper (we should probably reverse tried
-- to make a proper zipper)
tryJoin _ _ _ [] = Nothing
tryJoin a b tried (r:totry) = let
    p = pathVia a b tried (r:totry)
    next = tryJoin a b (tried++[r]) totry
    in shortestOf p next
    


-- | Find a path between source and destination via  (head totry)
pathVia:: String -- source
        -> String -- destination 
        -> [Join] -- tried
        -> [Join] -- to try
        -> Maybe [Join]

pathVia a b tried totry
    | a === b = Just [] -- found
    | totry == [] = Nothing
    | a === source r && b === destination r = Just [r]
    | a === source r = do 
        rs <- tryJoin (destination r) b [] (tried++totry')
        return (r:rs)
    | otherwise = Nothing
    where (r:totry') = totry

shortestOf :: Maybe ([Join]) -> Maybe ([Join]) -> Maybe ([Join])
shortestOf Nothing b = b
shortestOf a Nothing = a
shortestOf (Just a) (Just b) 
    | length a > length b = Just b
    | otherwise = Just a

--------------------------------------------------
-- | Find minimum subset linking starting points.
-- Probably not optimimal but we are only planning
-- to use is to join a few points (less that 10)
-- and all the other
-- The difficulty is to reuse existing path if possible to resolve
-- ambiguous case. See following diamon examples  :
-- a -> d should go via b, but
-- a -> c -> d should reuse c as a connection between a and d.
--
-- >>> :set -XDataKinds -XOverloadedStrings
-- >>> let a = "a.x" :: SQLFragment '[] '[]
-- >>> let b = "b.x"  :: SQLFragment '[] '[]
-- >>> let c = "c.x" :: SQLFragment '[] '[]
-- >>> let d = "d.x" :: SQLFragment '[] '[]
-- >>> let g = a !<->! b ++ b !->! d ++ a !<->! c ++ c !->! d
-- >>> let g' = g ++ (c !->! a)
-- >>> cover ["a", "d"] g
-- Just [Join {source = "a", destination = "b", joinClause = Just "((a.x)=(b.x))", joinType = INNER},Join {source = "b", destination = "d", joinClause = Just "((b.x)=(d.x))", joinType = INNER}]
-- >>> cover ["a", "c", "d"] g
-- Just [Join {source = "a", destination = "c", joinClause = Just "((a.x)=(c.x))", joinType = INNER},Join {source = "c", destination = "d", joinClause = Just "((c.x)=(d.x))", joinType = INNER}]
-- >>> cover ["c", "a", "d"] g
-- Just [Join {source = "c", destination = "a", joinClause = Just "((c.x)=(a.x))", joinType = INNER},Join {source = "c", destination = "d", joinClause = Just "((c.x)=(d.x))", joinType = INNER}]

cover :: [String] -> [Join] -> Maybe [Join]
cover [] _ = Just []
cover ts joins = cover' (Just []) ts joins

-- | Tries to find the shortest path between
-- a source set and the destination by accumulatiing
-- the found list. The source set is in fact a cached
-- version of all the table present in founds
cover' :: Maybe [Join] -- found joins
       -> [String] -- table left
       -> [Join]  -- join graph
       -> Maybe [Join]

cover' Nothing _ _ = Nothing
cover' founds [] joins = founds
-- cover' founds [a] joins = founds
cover' (Just []) (a:b:tables) joins = do
    found <-  (path a b joins)
    cover' (Just found) tables joins
cover' (Just founds) tables@(dest:dests) joins = let
    sources = tablesFrom founds
    paths =  (\a -> path a dest  joins) <$> sources :: [Maybe [Join]]
    shortestM = foldl1 shortestOf paths :: Maybe [Join]
    tablesFrom [] = []
    tablesFrom as@(a:_) = source a : map destination as
    in do 
        shortest <- shortestM
        cover' (Just $ founds ++ shortest) dests joins
    
-- * Operators and Builder

q !->! q' = [innerJoin (q !=! q')]
-- ^ Unidirectional inner join
q !*>! q' = [outerJoin (q !=! q')]
-- ^ Unidirectional outer join
q !-*>! q' = [leftJoin (q !=! q')]
-- ^ Unidirectional left join
q !*->! q' = [rightJoin (q !=! q')]
-- ^ Unidirectional right join

-- | Bidirectional inner join
q !<->! q' = [innerJoin (q !=! q'), innerJoin (q' !=! q)]
-- | Bidirectional left,right join
q !<-*>! q' = [leftJoin (q !=! q'), rightJoin (q' !=! q)]
-- | Bidirectional left,left join
q !<-*->! q' = [leftJoin (q !=! q'), leftJoin (q' !=! q)]
-- | Bidirectional outer join
q !<*>! q' = [outerJoin (q !=! q'), outerJoin (q' !=! q)]
{-
 -
!*->!
!<*->!
-}




-- (===) :: String -> String -> Bool
a === b = getAlias a == getAlias b where
    getAlias  = last . words
