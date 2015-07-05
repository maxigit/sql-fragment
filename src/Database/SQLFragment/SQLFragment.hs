{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{- LANGUAGE OverloadedStrings #-}
module Database.SQLFragment.SQLFragment
(
-- ** Types
  Section(..)
, SQLFragment(..)
-- ** Query Generations
, toSelectQuery
, toInsertQuery
-- , toUpdateQuery
-- , toDeleteQuery
-- ** Types Manipulation
-- | The following functions change the type signature of a fragment.
-- Combined with OverloadedStrings, they can be used to create a fragment from a string.
, forgetTypes
, clearTypes
, nullable
, notnull
-- ** Joins
, JoinType(..)
, Join(..)
, innerJoin
, leftJoin
, rightJoin
, outerJoin
, open
, openLeft
, openRight
, close
, closeLeft
, closeRight
, reverseJoinType
, inverseJoinType

-- ** Miscellaneous
, bracketize
, qualify
, aliasWithPrefix
, from
) where
-- standard
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String
import Data.List (intercalate)
import Data.Maybe(catMaybes, fromMaybe)
-- third party
import Text.Regex.PCRE ((=~))
-- local
import Database.SQLFragment.TypeFamilies

-- $setup
-- >>> let toInsertQuery' q = unwords . lines . toInsertQuery $ q
data Section = COLUMN
             | FROM
             | WHERE
             | GROUP
             | HAVING
             | ORDER
             deriving (Show, Read, Eq, Ord, Enum)

data QueryType = SELECT | INSERT | UPDATE | DELETE
     deriving (Show, Eq, Ord, Enum)

-- | Represents a bit of an SQL Query. Could be equivalent to
--
-- @
-- \"name FROM user\"
-- \"FROM user WHERE name = 'Smith'
-- \"GROUP BY name ORDER BY name\"
-- @
--
-- Fragments can be combined to form a full query.
-- However, the type of the query \"SELECT\", \"INSERT\" is not
-- carried by the fragment but decided when generating the query.
-- Fragments also carry some phantom types which are used
-- to type the query results and its parameters if any.
-- Phantom types allow type safe query composition even though
-- SQLFragment is designed to not be \"on the way\", and so
-- can be retyped easily.
--
-- Fragments can be left untyped by simply using "SQLFragment '[] '[]"
--
-- 'SQLFragment' can be constructing from a string using
-- the OverloadedStrings extension.
--
-- Accepted format are :
-- 
-- column
--
-- table.column
--
-- table [AS] alias.column
-- 
-- table.
--
-- table [AS] alis.
--
-- database.table.column
--
-- >>> :set -XOverloadedStrings
--
-- >>> toSelectQuery "a"
-- "SELECT a"
-- >>> toSelectQuery "t.a"
-- "SELECT t.a\nFROM t"
-- >>> toSelectQuery "t as t2."
-- "FROM t as t2"
-- >>> toSelectQuery "db.t as t2.a"
-- "SELECT t2.a\nFROM db.t as t2"
--
data SQLFragment (e :: [*]) (p :: [*])=  SQLFragment
    { clauses :: Map.Map Section [String]
    } deriving (Show, Eq)


instance Monoid (SQLFragment '[] '[]) where
    mempty = SQLFragment mempty
    -- | Join to fragment but keep the tables unique.
    -- However, tables need to be kept in their original order if 
    -- possible.
    mappend (SQLFragment clauses) (SQLFragment clauses') =
        SQLFragment ( Map.unionWithKey mergeClause clauses clauses') where
            mergeClause FROM ts ts' = joinTables ts ts'
            mergeClause _ ts ts' = mappend ts ts'

-- | Join tables by remove duplicated  and keeping the original
-- order if possible. Please note that the implementation
-- doesn't verify if the first list containts duplicates or not
-- In case of alias. Needs to keep the original alias definition
--
-- >>> joinTables [] ["a"]
-- ["a"]
-- >>> joinTables ["a"] []
-- ["a"]
-- >>> joinTables ["a","b","c"] ["c","d","b"]
-- ["a","b","c","d"]
-- >>> joinTables ["... a"] ["a", "b"]
-- ["... a","b"]
-- >>> joinTables ["a", "b"] ["... a"] :: [String]
-- ["b","... a"]

-- joinTables :: Eq a =>  [a] -> [a] -> [a]
joinTables as bs = let
    go as [] = as
    go as (b:bs) 
        | b `elem` as = go as bs
        | otherwise = go (as ++ [b])  bs

    -- remove bare alias if there is a definition for it
    -- ex in ["... a", "a",  "b"], "a" needs to be removed
    aliases = Set.fromList [a | t <- as ++bs
                              , let a = getAlias t
                              , a /= t
                            ]
    [as', bs'] = map (filter (`Set.notMember` aliases)) [as, bs]


    in go as' bs'

instance IsString (SQLFragment e p) where
    -- |
    fromString "" = SQLFragment Map.empty
    fromString s  = forgetTypes  (q :: SQLFragment '[] '[]) where
        q = case captures of 
            [] -> build COLUMN s
            [[],_,_, [], _] -> error $ "Can't parse " ++ (show s)
            [[], _, _, col, col'] -> build COLUMN (alias col col')
            [full_table, table, table', [], _] -> build FROM full_table
            [full_table, table, [], col, col'] ->
                build COLUMN (alias (qualify table col) col')
                `mappend` (build FROM full_table)
            [full_table, table, table', col, col'] ->
                build COLUMN (alias (qualify table' col) col')
                `mappend` (build FROM full_table)
            _ -> error $ show captures
        (_,_,_,captures) = matchColumnDef s
        qualify [] n = n
        qualify a n = a ++ "." ++ n
        alias n [] = n
        alias n a = n ++ " AS " ++ a
        
-- | Parse a column definition in the sahpe
--[[dabase].table [AS alias]].[column [AS alias]]
--return 5 captures
--   [[dabase].table [AS alias]].[column [AS alias]]
--   dabase.table
--   alias
--   columen
--   alias
--
matchColumnDef :: String -> (String, String, String, [String])
matchColumnDef s =  s =~ r where
    -- r = "^"++wspace++table++"\\."++column ++ wspace++"$"
    r = "^" ++ wspaces ++ table ++ "\\." ++ column ++ wspaces ++ "$"
    wspaces = "\\s*"
    column = "(?:" ++ identifier ++ ")?"
    identifier = "`?(\\w+)`?(?: (?:[aA][sS]\\s+)?(\\w+))?"
    table = "((?:\\w+\\.)?" ++ identifier ++")?" -- optionabl db + id
--------------------------------------------------
-- * Query generation
--------------------------------------------------
toSelectQuery :: SQLFragment e p -> String                     
toSelectQuery q =  toSQL SELECT q


-- | Insert using VALUES and ?
-- 
-- >>> toInsertQuery' $ "t.a" 
-- "INSERT INTO t (t.a) VALUES (?)"
toInsertQuery q = toSQL INSERT q
toUpdateQuery q = toSQL UPDATE q
toDeleteQuery q = toSQL DELETE q

-- | generique function to transform a Fragment to query (String)
toSQL :: QueryType -> SQLFragment e p -> String
toSQL qt q = intercalate "\n" (map (sectionToSQL qt) sections) where
    sections = catMaybes $ map get (sectionFor qt)
    get section = do
        ss <- Map.lookup section (clauses q)
        if ss == []
           then Nothing
           else Just (section, ss)

sectionToSQL :: QueryType ->  (Section, [String])  -> String
sectionToSQL INSERT (COLUMN, cols) =
    bracketize colNames 
    ++ "\nVALUES (" ++ params ++ ")" where
    colNames = intercalate (joinSymbol COLUMN) cols
    params = intercalate (joinSymbol COLUMN) $ map (const "?") cols

sectionToSQL qt (s, xs) = sectionName qt s ++ " " ++ (intercalate (joinSymbol s) (map (bracketizeIf s) xs))


bracketize :: String -> String
bracketize s = "(" ++ s ++ ")"

-- | Bracketize if needed (depending on the Section)
bracketizeIf :: Section -> String -> String
-- bracketizeIf COLUMN s = bracketize s
bracketizeIf WHERE s = bracketize s
bracketizeIf HAVING s = bracketize s

bracketizeIf _ s = s

joinSymbol :: Section -> String
joinSymbol WHERE = " AND " 
joinSymbol HAVING = " AND "
joinSymbol _ = ", "

sectionName :: QueryType -> Section -> String
sectionName qt COLUMN = show qt
sectionName _ GROUP = "GROUP BY"
sectionName _ ORDER = "ORDER BY"
sectionName INSERT FROM = "INSERT INTO"
sectionName _ section = show section

sectionFor SELECT = [COLUMN .. ORDER]
sectionFor INSERT = [FROM, COLUMN]
sectionFor UPDATE = [COLUMN .. WHERE]
sectionFor DELETE = [COLUMN .. WHERE]

--------------------------------------------------
-- * Construction
--------------------------------------------------
build :: Section -> String -> SQLFragment e p
build section value = SQLFragment (Map.singleton section [value])

from :: String -> SQLFragment '[] '[]
from = build FROM

-- | Alias a string with a prefix
--
-- >>> aliasWithPrefix "2" "table"
--"table table2"
aliasWithPrefix :: String -- ^ Alias suffix
                -> String -- ^ aliased
                -> String

aliasWithPrefix "" t = t
aliasWithPrefix s t = t ++ " " ++ t ++ s


-- | Extract the alias from  an /aliased/  string
--
-- >>> getAlias "table"
-- "table"
--
-- >>> getAlias "table alias"
-- "alias"
getAlias :: String -> String
getAlias = last . words
-- |
--
-- >>> qualify "table" "column"
-- "table.column"
qualify :: String -> String -> String
qualify "" b = b
qualify a b = a ++ "." ++  b


--------------------------------------------------
-- * Type Manipulation
--------------------------------------------------

-- | Forget the phantom types to whatever is needed.
--
-- >>> :set -XDataKinds
-- >>> let a = "a" :: SQLFragment '[] '[]
-- >>> let b = forgetTypes a :: SQLFragment '[String] '[]
forgetTypes :: SQLFragment e p -> SQLFragment e' p'
forgetTypes q = SQLFragment { clauses = clauses q }

-- Clear the phatome types to '[] '[]
clearTypes :: SQLFragment e p -> SQLFragment '[] '[]
clearTypes q = forgetTypes q
-- | Change types to Maybe version if needed
nullable :: SQLFragment e p -> SQLFragment (Nullable e) (Nullable p)
nullable = forgetTypes
notnull :: SQLFragment e p -> SQLFragment (NotNull e) (NotNull p)
notnull = forgetTypes

--------------------------------------------------
-- * Join
--------------------------------------------------
--
-- | Type of Join. At the moment only left and inner join are supported.
data JoinType = LEFT | RIGHT | INNER | OUTER deriving (Show, Eq, Ord, Enum)

-- | Join between 2 tables. 
--
data Join  = Join 
    { source :: String
    , destination :: String
    , joinClause :: Maybe String
    , joinType :: JoinType
    } deriving (Show, Eq)

-- | Create a join from SQLFragment. The fragment should have
-- at have exactly 2 tables and it's columns Bools.
makeJoin :: Boolables a => JoinType ->  SQLFragment a '[] -> Join
makeJoin t q = let
    tables = Map.lookup FROM (clauses q)
    clause = do
        cl <- Map.lookup COLUMN (clauses q)
        -- return $ sectionToSQL SELECT (COLUMN, cl)
        return $ intercalate (joinSymbol WHERE) (map (bracketizeIf WHERE) cl)
    in  case tables of 
        Just [src, dest] ->  Join src dest clause t
        otherwise -> error "needs exactly two tables"

reverse, openLeft, openRight, closeLeft, closeRight, open, close :: Join -> Join
reverse (Join src dst clause t) = Join dst src clause (reverseType t) where
    reverseType LEFT = RIGHT
    reverse t = t

leftJoin q = makeJoin LEFT q
rightJoin q = makeJoin RIGHT q
innerJoin q = makeJoin INNER q
outerJoin q = makeJoin OUTER q

openLeft =  updateJoinTypeWith openLeftT
openLeftT RIGHT = OUTER
openLeftT INNER = LEFT
openLeftT t = t

reverseJoinType =updateJoinTypeWith reverseJoinTypeT 
inverseJoinType =updateJoinTypeWith inverseJoinTypeT 
openRight = reverseJoinType.openLeft.reverseJoinType
open j = j { joinType = OUTER }

closeRight = inverseJoinType.openRight.inverseJoinType
closeLeft = inverseJoinType.openLeft.inverseJoinType
close j = j { joinType =  INNER }


-- | Reverse left and right
reverseJoinTypeT RIGHT = LEFT
reverseJoinTypeT LEFT = RIGHT
reverseJoinTypeT t = t 

-- | Inverse close/open states
inverseJoinTypeT RIGHT = LEFT
inverseJoinTypeT LEFT = RIGHT
inverseJoinTypeT INNER = OUTER
inverseJoinTypeT OUTER = INNER


updateJoinTypeWith f j =  j { joinType = f jt } where 
    jt = joinType j


