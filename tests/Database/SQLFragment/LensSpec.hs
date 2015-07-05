-- | Lens and Labels SQLFragment integration tests.
{-# LANGUAGE DataKinds, OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Database.SQLFragment.LensSpec where 
-- standard
-- third-party
import qualified Data.Tagged as T
import Data.Tagged (Tagged)
import Test.Hspec
import Test.HUnit hiding (Label)
import Data.HList.CommonMain (Label(..), hLens')
-- local
import Database.SQLFragment

-- * Basic DB 
-- We use the same schema as in JoinSpec but add labels to it
a' = "a.x" :: SQLFragment '[String] '[]
b' = "b.x" :: SQLFragment '[String] '[]
d' = "d.x" :: SQLFragment '[Double] '[]
a = "a.x" :: SQLFragment '[Tagged xL String] '[]
b = "b.y" :: SQLFragment '[Tagged yL String] '[]
c = "c.x" :: SQLFragment '[Tagged xL String] '[]
d = "d.z" :: SQLFragment '[Tagged zL Int] '[]

xL = Label :: Label "x"
x = hLens' xL
yL = Label :: Label "y"
y = hLens' yL
zL = Label :: Label "z"
z = hLens' zL

aGraph = 
    a !<->! b
    ++ a !<->! c
    ++ b !->! d
    ++ c !->! d

spec :: Spec
spec = do
    describe "Lens" $do
        it "should be a Num" $do
            d' + d' == d' + d'
        it "should be a Num" $do
            d + d == d+d


main :: IO ()
main = hspec spec
