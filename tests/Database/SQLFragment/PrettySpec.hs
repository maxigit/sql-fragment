-- | Lens and Labels SQLFragment integration tests.
{-# LANGUAGE DataKinds, OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Database.SQLFragment.PrettySpec  where 
-- standard
-- third-party
-- import qualified Data.Tagged as T
-- import Data.Tagged (Tagged)
import Test.Hspec
import Test.HUnit hiding (Label)
-- import Data.HList.CommonMain (Label(..), hLens')
-- local
import Database.SQLFragment
import Database.SQLFragment.Pretty

-- * Basic DB 
-- We use the same schema as in JoinSpec
a = "a.x" :: SQLFragment '[] '[]
b = "b.x" :: SQLFragment '[] '[]
c = "c.x" :: SQLFragment '[] '[]
d = "d.x" :: SQLFragment '[] '[]
aGraph = a !<->! b ++ a !<->! c
bGraph = b !->! d ++ c !->! d

spec :: Spec
spec = do
    describe "joinsToDot" $do
        it "should generate a graph" $do
            let exp = "digraph \"a\" {\n\
                \    node [shape=box]\n\
                \    \"a\" -> \"b\";\n\
                \    \"b\" -> \"a\";\n\
                \    \"a\" -> \"c\";\n\
                \    \"c\" -> \"a\";\n\
                \}"
            exp @=? joinsToDot "a" aGraph
        
        it "should generate graphs with different edge colors" $do
            let exp = "digraph \"ab\" {\n\
                \    node [shape=box]\n\
                \    \"a\" -> \"b\" [color=black];\n\
                \    \"b\" -> \"a\" [color=black];\n\
                \    \"a\" -> \"c\" [color=black];\n\
                \    \"c\" -> \"a\" [color=black];\n\
                \    \"b\" -> \"d\" [color=red];\n\
                \    \"c\" -> \"d\" [color=red];\n\
                \}"
            exp @=? joinsListToDot "ab" [aGraph, bGraph]

main :: IO ()
main = hspec spec
