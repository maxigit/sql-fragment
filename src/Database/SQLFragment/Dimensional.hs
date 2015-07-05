{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Add arithmetic operators to deal with Units.
-- @todo add unit conversion
module Database.SQLFragment.Dimensional where
-- standard
-- third party
import qualified Numeric.Units.Dimensional.Prelude as Dim
import qualified Numeric.Units.Dimensional as Dim
import Data.Tagged (Tagged)
-- local
import Database.SQLFragment.SQLFragment
import Database.SQLFragment.Operators

-- * Arithmetic operators for SQLFragments holding dimensional units and quantities
-- | Addition  Subtraction
class Addable a b c | a b -> c where
    (!+!) :: a -> b -> c
    (!-!) :: a -> b -> c

instance (Num n) => Addable  (SQLFragment '[Dim.Dimensional v a n] '[])
                             (SQLFragment '[Dim.Dimensional v a n] '[])
                             (SQLFragment '[Dim.Dimensional v a n] '[]) where
    q !+! q' = forgetTypes $ "$1+$2" !%! q !%! q' !#:! q
    q !-! q' = forgetTypes $ "$1-$2" !%! q !%! q' !#:! q
    
instance (Num n) => Addable  (SQLFragment '[Tagged l (Dim.Dimensional v a n)] '[])
                             (SQLFragment '[Tagged l' (Dim.Dimensional v a n)] '[])
                             (SQLFragment '[Tagged l (Dim.Dimensional v a n)] '[]) where
    q !+! q' = forgetTypes $ "$1+$2" !%! q !%! q' !#:! q
    q !-! q' = forgetTypes $ "$1-$2" !%! q !%! q' !#:! q

instance (Num n, v ~ Dim.DQuantity) => Addable  (Dim.Dimensional v a n)
                             (Dim.Dimensional v a n)
                             (Dim.Dimensional v a n) where
    a !+! b = a Dim.+ b
    a !-! b = a Dim.- b


-- | Multiplication
class Multipliable a b c | a b -> c where
    (!*!) :: a -> b -> c

instance (Num n, Dim.Mul a b c) => Multipliable  (SQLFragment '[Dim.Dimensional v a n] '[])
                                                 (SQLFragment '[Dim.Dimensional v b n] '[])
                                                 (SQLFragment '[Dim.Dimensional v c n] '[]) where
    q !*! q' = forgetTypes $ "$1*$2" !%! q !%! q' !#:! q

instance (Num n, Dim.Mul a b c) => Multipliable  (SQLFragment '[Tagged l (Dim.Dimensional v a n)] '[])
                                                 (SQLFragment '[Tagged l' (Dim.Dimensional v b n)] '[])
                                                 (SQLFragment '[Tagged l (Dim.Dimensional v c n)] '[]) where
    q !*! q' = forgetTypes $ "$1*$2" !%! q !%! q' !#:! q


instance (Num n, Dim.Mul a b c) => Multipliable (Dim.Dimensional v a n)
                                                (Dim.Dimensional v b n)
                                                (Dim.Dimensional v c n) where
    a !*! b = a Dim.* b

-- | Division
class Divisible a b c | a b -> c where
    (!/!) :: a -> b -> c

instance (Num n, Dim.Div a b c) => Divisible  (SQLFragment '[Dim.Dimensional v a n] '[])
                                                 (SQLFragment '[Dim.Dimensional v b n] '[])
                                                 (SQLFragment '[Dim.Dimensional v c n] '[]) where
    q !/! q' = forgetTypes $ "$1*$2" !%! q !%! q' !#:! q

instance (Num n, Dim.Mul a b c) => Divisible  (SQLFragment '[Tagged l (Dim.Dimensional v a n)] '[])
                                                 (SQLFragment '[Tagged l' (Dim.Dimensional v b n)] '[])
                                                 (SQLFragment '[Tagged l (Dim.Dimensional v c n)] '[]) where
    q !/! q' = forgetTypes $ "$1*$2" !%! q !%! q' !#:! q


instance (Fractional n, Dim.Div a b c, v ~ Dim.DQuantity) => Divisible (Dim.Dimensional v a n)
                                             (Dim.Dimensional v b n)
                                             (Dim.Dimensional v c n) where
    a !/! b = a Dim./ b
-- * Export Dimensional operators

class Quantify a u q  where
    (!*) :: a -> u -> q

instance (Num a) => Quantify (SQLFragment '[a] p) 
                             (Dim.Unit d a )
                             (SQLFragment '[Dim.Quantity d a] p) where
    q !*  u = forgetTypes $ q
instance (Num a) => Quantify (SQLFragment '[Tagged l a] p) 
                             (Dim.Unit d a )
                             (SQLFragment '[Tagged l (Dim.Quantity d a)] p) where
    q !*  u = forgetTypes $ q

instance (Num a) => Quantify a (Dim.Unit d a) (Dim.Quantity d a) where
    a !* u  = a Dim.*~ u

class Dequantify a u q | u q -> a where
    (!/) :: q -> u -> a

instance (Fractional a) => Dequantify (SQLFragment '[a] p) 
                             (Dim.Unit d a )
                             (SQLFragment '[Dim.Quantity d a] p) where
    q !/  u = forgetTypes $ q
instance (Fractional a) => Dequantify (SQLFragment '[Tagged l a] p) 
                             (Dim.Unit d a )
                             (SQLFragment '[Tagged l (Dim.Quantity d a)] p) where
    q !/  u = forgetTypes $ q

instance (Fractional a) => Dequantify a (Dim.Unit d a) (Dim.Quantity d a) where
    a !/ u  = a Dim./~ u
{-
a *. b = a Dim.*~ b
a .* b = b Dim.*~ a
a /. b = a Dim./~ b


-}
-- test
x = "5" :: SQLFragment '[Dim.Quantity Dim.DLength Double] '[]



