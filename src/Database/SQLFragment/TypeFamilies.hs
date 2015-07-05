{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds,  TypeOperators #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- | Defines common type families 
module Database.SQLFragment.TypeFamilies where

-- standard
-- thir-party
import Data.Tagged (Tagged)
import Data.Proxy (Proxy)
-- local

-- | Remove the `Maybe` of a Type
type family UnMaybe a where
    UnMaybe (Maybe a) = a
    UnMaybe a = a

-- | Remove `Maybe` on all types of a list
type family NotNull (as :: [*]) where
    NotNull '[] = '[]
    NotNull (a ': as) = UnMaybe a ': NotNull as

-- | Add `Maybe` on all types of a list if necessary
type family Nullable (as :: [*]) where
    Nullable '[] = '[]
    Nullable (a ': as) = Maybe (UnMaybe a) ': Nullable as

-- | Wrap a type in a `Maybe` if the source type is a `Maybe`
-- Usefull to *convert* types whilst preserving the maybenes.
-- Example, in SQL the type of "$1>0" !%! "hello" is '[Bool]
-- or '[Maybe Bool] if the value is nullable. 

type family IfMaybe a b where
    IfMaybe (Maybe a) b = Maybe b
    IfMaybe a b = b

-- | Wrap a destination type in a `Maybe if one of the
-- source types is also a `Maybe`.
type family IfMaybe2 a b c where
    IfMaybe2 (Maybe a) b c = Maybe c
    IfMaybe2 a (Maybe b) c = Maybe c
    IfMaybe2 a b c = c


-- | Extract labels (proxy)  of list of types
type family GetLabels (as :: [*]) where
    GetLabels '[] = '[]
    GetLabels ((Tagged l a) ': as) = a ': GetLabels as
    GetLabels (a ': as) = GetLabels as

type family GetLabel a :: k where
    GetLabel (Tagged (l :: k) a)  = l
    GetLabel a = ()
    
type family GetValues (as :: [*]) where
    GetValues '[] = '[] 
    GetValues (a ': as) = GetValue a ': GetValues as

type family GetValue a where
    GetValue (Tagged (l :: k)  a)  = a
    GetValue a = a


class Boolable b
instance Boolable Bool
instance Boolable (Maybe Bool)
instance Boolable b => Boolable (Tagged l b)

class Boolables (bs :: [*] )

-- instance (Boolable a) => Boolables '[a]
instance Boolables '[]
instance (Boolable a, Boolables as) => Boolables (a ': as)

-- * List retypings
-- | 

type family RelabelList (ls :: [*]) (ts :: [*]) where
    RelabelList '[] t = RelabelList '[] t
    RelabelList l '[] = RelabelList l '[]
    RelabelList ((Tagged s b) ': ls) (t ': ts) = Tagged s t ': (RelabelList ls ts)
    
