-- | Pretty print functions to display fragments and
-- generate dot graph for join graph.
module Database.SQLFragment.Pretty 
(
-- * Graph
-- ** Types
Attribute
-- ** Rendering functions
, joinsToDot
, joinsListToDot
)  where

-- standard
-- third-party
import qualified Text.PrettyPrint as P
import Text.PrettyPrint (Doc, (<>), (<+>))
-- local
import Database.SQLFragment.SQLFragment

-- Generates a graph from a list of `Join`.
joinsToDotWith :: RenderOption -- ^ option
           -> String -- ^ graph name 
           ->  [Join]
           -> String
joinsToDotWith opts name js = docToDot name (joinsToDoc opts js)

joinsToDot :: String
           -> [Join]
           -> String

joinsToDot = joinsToDotWith defOptions
            

docToDot :: String -> Doc -> String
docToDot name doc = show $ P.vcat 
    [ P.text ("digraph " ++ show name) <+> P.lbrace
    , P.nest 4 doc'
    , P.rbrace
    ]
    where doc' =  P.vcat
            [ P.text "node" <+> attributesToDoc [("shape", "box")]
            , doc
            ]

joinsToDoc :: RenderOption
           -> [Join]
           -> Doc
joinsToDoc opts js = P.vcat $ map (joinToDoc a2d) js where
    a2d = renderEdge opts

joinToDoc :: (Join -> [Attribute]) ->  Join -> Doc
joinToDoc a2d j = P.hsep
    [ P.text (show $ source j) 
    , P.text "->"
    , P.text (show $ destination j)
    , attributesToDoc (a2d j)
    ] <> P.semi

joinsListToDot :: String  -- ^ graph name
               -> [[Join]]
               -> String
joinsListToDot = joinsListToDotWith opts where
    -- opts = repeat defOptions
    opts = cycle [defOptions {renderEdge = color c}
                 | c <- colors
                 ]

joinsListToDotWith :: [RenderOption]
                   -> String -- ^ graph name
                   -> [[Join]]
                   -> String

joinsListToDotWith opts name jss = docToDot name (P.vcat docs)
    where docs = zipWith joinsToDoc opts jss

type Attribute = (String, String)
attributesToDoc :: [Attribute] -> Doc
attributesToDoc [] = P.empty
attributesToDoc atts = P.brackets (P.hcat (map render atts)) where
    render (k,v) = P.hcat [P.text k, P.equals, P.text v]

data RenderOption = RenderOption 
    { renderEdge :: Join -> [Attribute]
    , renderNode :: String -> [Attribute]
    }

defOptions :: RenderOption
defOptions = RenderOption (const []) (const [])

color :: String -> Join -> [Attribute]
color c = const [("color", c)] 

colors = words "black red blue darkgreen purple brown cyan green"
