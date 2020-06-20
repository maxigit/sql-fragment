{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.SQLFragment.Operators where
-- standard
import Data.Monoid
import Data.List (intercalate)
import Data.String
import GHC.TypeLits
-- third-party
import Data.HList
-- local
import Database.SQLFragment.SQLFragment
import Database.SQLFragment.Internal
import Database.SQLFragment.TypeFamilies

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XDataKinds
-- 
-- >>> let toSelectQuery' q = unwords.lines.toSelectQuery $ q
--

-- * Combination
infixl 1 !&!, !::!, !:@!, !::#!
infixl 3 !\!, !%!, !%%!
infixl 4 !>!, !<!, !>=!, !<=!, !=!, !/=!, !&&!, !||!, !=%!, !=~!
infixl 4 !>?!, !<?!, !>=?!, !<=?!, !=?!, !/=?!, !&&?!, !||?!, !=%?!, !=~?!
infixl 8 !#!, !#:!, !#:@!, !#:#!

-- | Combines two fragments. Similar to '<>' but combines
-- phantom types as well.
--
-- >>> toSelectQuery' $ "a" !&! "t."
-- "SELECT a FROM t"
-- >>> toSelectQuery' $ "a" !&! "b"
-- "SELECT a, b"
-- >>> toSelectQuery' $ "t.a1" !&! "s.b" !&! "t.a2"
-- "SELECT t.a1, s.b, t.a2 FROM t, s"

(!&!) :: SQLFragment e p
      -> SQLFragment e' p'
      -> SQLFragment (HAppendListR e e') (HAppendListR p p')

q !&! q' = forgetTypes $ (clearTypes q) <> (clearTypes q')

-- | Same as !&! but higher fixity
(!#!) :: SQLFragment e p
      -> SQLFragment e' p'
      -> SQLFragment (HAppendListR e e') (HAppendListR p p')

q !#! q' = forgetTypes $  q !&! q'

-- * Columns Promotions
--
-- | Promotes columns to the WHERE clause
--
-- >>> toSelectQuery' $ "" !\! "a > 1" !#! "b > 1" 
-- "WHERE (a > 1) AND (b > 1)"
-- >>> toSelectQuery' $  "a" !\! "b > 1"
-- "SELECT a WHERE (b > 1)"
--
(!\!) :: SQLFragment a  b -> SQLFragment a' b' -> SQLFragment a (HAppendListR b b')
q !\! q' = forgetTypes $   q !&! q0' !&! q'' where
    q0' = clearSection COLUMN q'
    q'' = pickSection COLUMN WHERE q'

-- | Promotes columns to the HAVINg clause.
--
-- >>> toSelectQuery' $  "a" !\\! "b > 1"
-- "SELECT a HAVING (b > 1)"
--
(!\\!) :: SQLFragment a  b -> SQLFragment a' b' -> SQLFragment a (HAppendListR b b')
q !\\! q' = forgetTypes $   q !&! q0' !&! q'' where
    q0' = clearSection COLUMN q'
    q'' = pickSection COLUMN HAVING q'

-- | Promotes columns to GROUP clause and
-- criteria to HAVING Clauses
--
-- >>> toSelectQuery' $ "t.a" !~! ("a" !\! "b > 1")
-- "SELECT t.a FROM t GROUP BY a HAVING (b > 1)"
--
(!~!) :: SQLFragment a b -> SQLFragment a' b' -> SQLFragment a (HAppendListR b b')
q !~! q' = forgetTypes$ q !&! q0' !&! q'' where
    q0' = clearSection COLUMN . clearSection WHERE $ q'
    q'' = pickSection COLUMN GROUP q' !&! pickSection WHERE HAVING q'

-- | Promotes columns to ORDER BY clause (Ascending)
--
-- >>> toSelectQuery' $ "t.a" !^! "order"
-- "SELECT t.a FROM t ORDER BY order"
--
(!^!) :: SQLFragment a b -> SQLFragment a' '[] -> SQLFragment a b 
q !^! q' = forgetTypes$ q !&! qorder where
        qorder = pickSection COLUMN ORDER q'

-- | Promotes columns to ORDER BY clause (Descending)
--
-- >>> toSelectQuery' $ "t.a" !^-! "order"
-- "SELECT t.a FROM t ORDER BY order DESC"
--
(!^-!) :: SQLFragment a b -> SQLFragment a' '[] -> SQLFragment a b 
q !^-! q' = q !^! (fromString "$1 DESC" !%! q')

-- * Combining  & Formating
-- | Replaces "$n" with columns. Can be chained to combined
-- 2 or more columns into one.
--
-- >>> toSelectQuery' $ "IF($1>0, $1, 0)" !%! "t.a"
-- "SELECT IF(t.a>0, t.a, 0) FROM t"
-- >>> let a = forgetTypes $ "a1" !#! "b1" :: SQLFragment  '[String, Double] '[]
-- >>> let b =  forgetTypes $ "a2" !#! "b2" :: SQLFragment '[String, Double] '[]
-- >>> toSelectQuery' $ "$1 > $2" !%! a !%! b
-- "SELECT a1 > a2, b1 > b2"
(!%!) :: SQLFragment e '[] -> SQLFragment e '[] -> SQLFragment e '[]
q !%! q' = forgetTypes $ setSection COLUMN formatted (q !&! q') where
    formatted = zipWith format
                        (cycle $ getSection COLUMN q)
                        (getSection COLUMN q')
-- | Replace $1 with all the column together.
--
-- >>> toSelectQuery' $ "DISTINCT $1" !%%! "a" !#! "b"
-- "SELECT DISTINCT a, b"
(!%%!) :: SQLFragment e p -> SQLFragment e p -> SQLFragment e p
q !%%! q' = forgetTypes $ setSection COLUMN formatted q where
    cols = getSection COLUMN q'
    cols' = intercalate ", " cols
    formatted = zipWith format
                        (cycle $ getSection COLUMN q)
                        [cols']

-- ** Arithemitic operators
internOperator1 :: String->  SQLFragment e '[] -> SQLFragment e' '[]
internOperator1 op  q = forgetTypes $ fromString op !%! q

internOperator2 :: String->  SQLFragment e '[] -> SQLFragment e '[] -> SQLFragment e '[]
internOperator2 op  q q' = forgetTypes $ fromString op !%! q !%! q'

-- SQLFragment are also number, allowings for example
-- >>> "count" * 5
-- | List of Num.
class Nums (a::[*])
instance Nums '[]
instance (Num a, Nums as) => Nums (a ': as)

instance Nums a => Num (SQLFragment a '[]) where
-- instance Num (SQLFragment a '[]) where
    (+) = internOperator2 "$1+$2"
    (*) = internOperator2 "$1*$2"
    (-) = internOperator2 "$1-$2"
    negate = internOperator1 "-$1"
    abs = internOperator1 "ABS($1)"
    signum = error "`signum` not defined for `SQLFragment`"
    fromInteger a = fromString $ show a 

instance Fractionals a => Fractional (SQLFragment a '[]) where
    (/) =  internOperator2 "$1/$2"
    recip a = 1/a --  internOperator1 "1/$1" a
    fromRational a = fromString $ show a
 
-- | List of Fractional.
class Nums a => Fractionals (a :: [*])
instance Fractionals '[]
instance (Fractional a, Fractionals as) => Fractionals (a ': as)

-- ** Comparaison Operators

-- | columns comparaison operators. Note the result is a column
-- and has to be promoted to a where clause if necessary.
--
-- >>> let a = "a" :: SQLFragment '[String, Maybe Double, String] '[]
-- >>> let b = "b" :: SQLFragment '[Maybe String, Double, String] '[]
-- >>> :t a !>! b
-- a !>! b :: SQLFragment '[Maybe Bool, Maybe Bool, Bool] '[]
-- >>> toSelectQuery$ a !>! b
-- "SELECT (a)>(b)"
-- >>> let x = "x" :: SQLFragment '[String] '[]
-- >>> let y = "y" :: SQLFragment '[Int] '[]
-- >>> toSelectQuery'$ x !\! y !>! "5"
-- "SELECT x WHERE ((y)>(5))"
(!>!) = boolOperator ">"
(!<!) = boolOperator "<"
(!>=!) = boolOperator ">="
(!<=!) = boolOperator "<="
(!=!) = boolOperator "="
(!/=!) = boolOperator "!="
(!&&!) = boolOperator " AND "
(!||!) = boolOperator " OR "
(!=%!) = boolOperator " LIKE "
(!=~!) = boolOperator " RLIKE "
boolOperator :: String
             -> SQLFragment a '[]
             -> SQLFragment b '[]
             -> SQLFragment (ZipToBool a b) '[]
boolOperator op q q' = forgetTypes$ fromString ("($1)"++op++"($2)")  !%! q0 !%! q0' where
    q0 = clearTypes q
    q0' = clearTypes q'

-- ** Comparaison
-- | Comparaison operators with external parameters
-- Note, the result is promoted as in where clause.
--
-- >>> let a = "a" :: SQLFragment '[String, Maybe Double, String] '[]
-- >>> let b = "b" :: SQLFragment '[String] '[]
-- >>> :t a !>?! b
-- a !>?! b :: SQLFragment '[String, Maybe Double, String] '[String]
-- >>> toSelectQuery' $ a !>?! b
-- "SELECT a WHERE ((b)>(?))"
(!>?!) = buildBoolParamOp (!>!)
(!<?!) = buildBoolParamOp (!<!)
(!>=?!) = buildBoolParamOp (!>=!)
(!<=?!) = buildBoolParamOp (!<=!)
(!=?!) = buildBoolParamOp (!=!)
(!/=?!) = buildBoolParamOp (!/=!)
(!&&?!) = buildBoolParamOp (!&&!)
(!||?!) = buildBoolParamOp (!||!)
(!=%?!) = buildBoolParamOp (!=%!)
(!=~?!) = buildBoolParamOp (!=~!)

buildBoolParamOp :: (SQLFragment p' '[]
                        -> SQLFragment b '[]
                        -> SQLFragment (ZipToBool p' b) '[])
                 -> SQLFragment e p
                 -> SQLFragment p' '[]
                 -> SQLFragment e (HAppendListR p p')
buildBoolParamOp op q q' = forgetTypes $ q !\! op q' (fromString "?")


type family ZipToBool (a :: [*]) (b :: [*]) :: [*] where
    ZipToBool '[] '[] = '[]
    ZipToBool ((Tagged l a) ': as) ((Tagged l' b) ': bs) = Tagged l (IfMaybe2 a b Bool) ': ZipToBool as bs
    ZipToBool (a ': as) (b ': bs) = IfMaybe2 a b Bool ': ZipToBool as bs

-- * Type operations
-- ** Full type
-- | Retype a fragment with the right one
(!::!) :: SQLFragment e p -> SQLFragment e' p' -> SQLFragment e' p'
q !::! _ = forgetTypes q

-- | Same as !::! but with higher fixity
(!#:!) :: SQLFragment e p -> SQLFragment e' p' -> SQLFragment e' p'
q !#:! _ = forgetTypes q


-- ** Label
-- | Label a fragment with the given label
(!:@!) :: SQLFragment  '[a] p -> Label (l' :: Symbol) -> SQLFragment '[Tagged l' (GetValue a)] p
q !:@! _ = forgetTypes q
-- | Same as !:@! but with hight fixity
(!#:@!) :: SQLFragment  '[a] p -> Label (l' :: Symbol) -> SQLFragment '[Tagged l' (GetValue a)] p
q !#:@! _ = forgetTypes q

-- **  Underlying Type
-- | Change the underlying type but keep the actual label
(!::#!) :: SQLFragment '[Tagged (l::Symbol) a] p -> SQLFragment '[b] '[] -> SQLFragment '[Tagged l (GetValue b)] p
q !::#! _  = forgetTypes q
(!#:#!) :: SQLFragment '[Tagged (l:: Symbol) a] p -> SQLFragment '[b] '[] -> SQLFragment '[Tagged l (GetValue b)] p
q !#:#! _ = forgetTypes q

