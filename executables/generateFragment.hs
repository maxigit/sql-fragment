 -- | Generates 'SQLFragment' from a tsv file (tab separated).
-- The input file format is
-- - haskell column name (must be unique with the file).
-- - SQL table
-- - SQL column
-- - haskell type
-- - nullable (True|False)
--
-- Example 
-- | customer'name | customer | name | String | False | customer_name |
--
-- main version:
-- customer'name = "customer.name" :: SQLFragment '[String] '[]
-- aliasable:
-- customer'name2 = customer'name_ 2 --  "customer customer2.name :: SQLFragment '[String] '[] 
--
-- With label
-- main version:
-- type NameLabel = Label "name"
-- customer'name = "customer.name" :: SQLFragment '[Tagged NameLabel String] '[]
-- name = Label :: LabelName
--
-- Lens:
-- Instead of creating a label/lens per field we only create one
-- per column. The label are not carried by the SQLFragment but 
-- have to be set explicitly when fetching the records.
-- The advantage of that is the same label can be used for different
-- query having the same semantic.
-- name = hLens' LabelName
--
-- with/without Maybe (M=Maybe/U=UnMaybe)
-- Nullable types are wrapped in a Maybe, to force bare type use '
-- customer'nameU = "customer.name" :: SQLFragment '[String] '[]
-- customer'nameM = "customer.name" :: SQLFragment '[Maybe String] '[]
--

module Main (main) where
-- standard
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
 
import System.IO (readFile)
import System.Console.GetOpt as O
import Data.Char (toUpper)
import Control.Monad
import Data.Maybe
import Data.List (nub, intercalate)
import Data.Function(on)
import Data.Monoid(mappend)
-- third party
import qualified Data.Map as Map
-- local

data Options = Options
    { oFile :: String
    , oModuleName :: Maybe String
    , oHeaderName :: Maybe String
    , oGenerateLabels :: Bool
    , oGenerateLenses :: Bool
    , oFullTable :: Bool
    , oDatabase :: Maybe String
    } deriving Show

needsLabels o = oGenerateLabels o || oGenerateLenses o


defaultOptions = Options
    { oFile = ""
    , oModuleName = Nothing
    , oHeaderName = Nothing
    , oGenerateLabels = False
    , oGenerateLenses = False
    , oFullTable = False
    , oDatabase = Nothing
    }

options = 
    [ O.Option "n" ["name"] 
       ( setIf (\o a -> o {oModuleName = Just a})  "MODULE_NAME")
        "module name"
    , O.Option "i" ["header"]
       ( setIf (\o a -> o {oHeaderName = Just a}) "HEADER_FILE_NAME")
        "header file name"
    , O.Option "d" ["database"]
       ( setIf (\o a -> o {oDatabase = Just a}) "DATABASE")
        "database name"
    , O.Option "s" ["lens"]
        ( O.NoArg (\o -> o {oGenerateLenses = True})) "generate lenses"
    , O.Option "l" ["labels"]
        ( O.NoArg (\o -> o {oGenerateLabels = True})) "generate labels"
    , O.Option "f" ["full"]
        ( O.NoArg (\o -> o {oFullTable = True})) "generate full tables"
    ]
    where setIf f s = O.OptArg (\arg opt ->  maybe opt (f opt) arg) s

main :: IO ()
main = do
    args <- getArgs
    case processArg args of
        (Left errors) ->  error$ concat errors ++ O.usageInfo header options
        (Right opt) -> generateFragments opt
    where header = "Usage: generate-fragments: [OPTIONS] file"

processArg :: [String] -> Either [String] Options
processArg args = case O.getOpt O.Permute options args of
    (actions, [file], []) -> Right $ foldl (flip id) defaultOptions { oFile=file } actions
    (_, _, errors) -> Left errors

generateFragments :: Options -> IO ()
generateFragments options  = do
    content <- readFile (oFile options)
    let rows = lines content
    generateHeader options
    mapM_ (generateField options) rows
    when (needsLabels options) $ do
        labels <-  generateLabels options rows
        return ()
    when (oFullTable options) $ do
        generateFullTables options rows

generateHeader  options = do
    putStrLn$ "{-# LANGUAGE DataKinds, TypeOperators #-}"
    when (oGenerateLenses options)
        (putStrLn "{-# LANGUAGE NoMonomorphismRestriction #-}")
    putStrLn$ "-- | This module has been generated by 'generateFragment'"
    putStrLn$ "-- Please do not modify directly, but modify its source '"++(oFile options)++" instead."
    putStrLn$ "module "++name++" where"
    putStrLn$ ""
    putStrLn$ "-- standard"
    putStrLn$ "import qualified Data.Time as Time"
    putStrLn$ "import GHC.TypeLits"
    putStrLn$ "import Data.String(fromString)"
    putStrLn$ "-- third party"
    when (needsLabels options)$ do
         putStrLn "import Data.HList.CommonMain (Label(..))"
         putStrLn "import Data.Tagged (Tagged)"
    when (oGenerateLenses options)$
         putStrLn "import Data.HList.CommonMain (hLens')"
    putStrLn$ "import Database.SQLFragment"
    putStrLn$ ""
    when (isJust (oHeaderName options))$ do
        putStrLn$ "-- BEGIN Included header"
        contents <- readFile  (fromJust (oHeaderName options))
        putStr contents
        putStrLn$ "-- END Included header"
        putStrLn$ ""

    where name =  maybe (capitalize base) id (oModuleName options)
          base = takeBaseName (oFile options)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper(x):xs



    
generateField :: Options -> String -> IO ()
generateField options line = case (words line) of 
    [haskell, table, column, htype, nullable, label] -> do
        let table' = qualify (oDatabase options) table 
            aliaser = haskell ++ "_"
            htype' = labellize (needsLabels options) (if (read nullable) then "(Maybe "++htype++")" else  htype)
            signature = "SQLFragment '[" ++ htype' ++ "] '[]"
            labellize False t = t
            labellize True t = "Tagged "++ show label ++ t
        putStrLn "--------------------------------------------------"
        putStrLn$ "-- "++ line
        putStrLn "--------------------------------------------------"
        -- generate aliaser
        putStrLn $ aliaser ++ " :: String -> " ++ signature 
        putStrLn $ aliaser ++ " s = fromString $  qualify (aliasWithPrefix s "
                           ++ show table'
                           ++ ") "
                           ++ show column
        -- generate main column
        putStrLn $ haskell++ " :: " ++ signature
        putStrLn $ haskell++ " = " ++ aliaser ++ show ""
        putStrLn ""
        putStrLn ""
    otherwise -> error $ "Wrong number of parameter for line : " ++ line
    where 
          qualify (Just db) = ((db++".")++)
          qualify Nothing = id
        
        

            
generateLabels :: Options -> [String] -> IO ()
generateLabels options lines =  mapM_ generateLabelsForColumn' uniqueLabels where
    useLens = oGenerateLenses options
    uniqueLabels = nub [label | [_,_,_,_,_,label] <- map words lines]
    generateLabelsForColumn' label = do
        putStrLn "--------------------------------------------------"
        putStrLn$ "-- " ++ label 
        putStrLn "--------------------------------------------------"
        labelName <- generateLabel label
        when useLens $ do
            generateLens label labelName
        putStrLn ""
        putStrLn ""
    
generateLabel label = do
    let label' = toIdentifier $  label ++ "Label"
    putStrLn$ label' ++ " = Label :: Label " ++ show label
    return label'
generateLens lens label =
    putStrLn$ toIdentifier lens ++ " = hLens' " ++ label

-- | 
generateFullTables :: Options -> [String] -> IO ()
generateFullTables options lines = mapM_ generateFullTable (Map.toList tables) where
    tables = Map.fromListWith mappend [(table row, [row]) | row <- (map words lines)]
    table [haskell, table', column, htype, nullable, label] = table'
    haskell [haskell', table, column, htype, nullable, label] = haskell'
    generateFullTable (table, rows)  = do 
        putStrLn "--------------------------------------------------"
        putStrLn $ "-- All columns for " ++ table
        putStrLn "--------------------------------------------------"
        putStrLn $ (prefix (map haskell rows)) ++ "all = " ++ (intercalate " !&! " (map haskell rows))
        putStrLn ""

    prefix = foldl1 common  
    common [] _ = []
    common _ [] = []
    common (x:xs) (y:ys)
        | x == y = x:common xs ys
        | otherwise = []

    
{-
generateLabelsForColumn useLens suffix label = do
    putStrLn$ label ++ " = Label :: Label " ++ show label
    when (useLens) (putStrLn$ lens ++ " =  hLens' " ++ label)
    putStrLn ""
    putStrLn ""
    return (lens, label)

    where base = (if column `elem` keywords then ("_") else "") ++ label -- capitalize column
          lens = base
-}

-- | Convert a string to a valid Haskell identifier if needed
toIdentifier :: String -> String
toIdentifier s  = (if s `elem` keywords then ("_") else "") ++ s
keywords = words "base type class default"
