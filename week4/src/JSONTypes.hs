module JSONTypes 
    ( JValue(..)
    , mkJPair
    , mkJObj
    , jsonParser
    )
where

import Data.Map hiding ( map )
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor.Identity

type JMap = Data.Map.Map String JValue
data JValue = JString String
        | JNumber Integer
        | JObject JMap
        | JArray [JValue]
        | JBool Bool
        | JNull 
        deriving (Show)

mkJPair k v = JObject (Data.Map.singleton k v)

mkJObj :: [JValue] -> JValue
mkJObj j_vals = 
    let
        list_of_maps = map (\(JObject pair) -> pair) j_vals
        combined_map = Data.Map.unions list_of_maps
    in
        JObject combined_map



lexer         = P.makeTokenParser emptyDef
whiteSpace    = P.whiteSpace lexer
brackets      = P.brackets lexer
commaSep      = P.commaSep lexer
symbol        = P.symbol lexer
stringLiteral = P.stringLiteral lexer 
colon         = P.colon lexer
braces        = P.braces lexer

jsonArrayParser :: Parser JValue
jsonArrayParser = do    
    jVals <- brackets $ commaSep jsonValueParser 
    return $ JArray jVals

jsonObjParser :: Parser JValue
jsonObjParser = do
    j_vals <- braces $ commaSep jsonPairParser -- a list of pairs
    return $ mkJObj j_vals

jsonBoolParser = do
    bstr <- symbol "true" <|> symbol "false"
    return $ JBool (if bstr == "true" then True else False)

jsonStringParser :: Parser JValue
jsonStringParser = undefined

jsonNumberParser :: Parser JValue
jsonNumberParser = undefined

jsonNullParser :: Parser JValue
jsonNullParser = undefined

jsonPairParser :: Parser JValue
jsonPairParser = do
    k <- stringLiteral
    colon
    v <- jsonValueParser
    return $ mkJPair k v 

jsonValueParser :: Parser JValue
jsonValueParser = 
    jsonArrayParser <|> 
    jsonObjParser <|> 
    jsonStringParser <|> 
    jsonNumberParser <|> 
    jsonBoolParser <|> 
    jsonNullParser
    
jsonParser :: Parser JValue
jsonParser = do
    whiteSpace
    jsonArrayParser <|> jsonObjParser