module ShowParser ( parseShow, quoted_string ) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec
import Data.Functor.Identity

xml_header :: String
xml_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

parseShow :: String -> String
parseShow s = xml_header ++ (run_parser showParser s)

run_parser :: Parser a -> String -> a
run_parser p str =  case parse p "" str of
    Left err -> error $ "parse error at " ++ (show err)
    Right val  -> val

otag :: String -> String
otag t = "<" ++ t ++ ">"

ctag :: String -> String
ctag t = "</" ++ t ++ ">"

tag :: String -> String -> String
tag t v = concat [otag t, v, ctag t]

tagAttrs :: String -> [(String, String)] -> String -> String
tagAttrs t attrs v = concat [otag (t ++ " " ++ atrstr), v, ctag t]
  where
    atrstr = unwords $ map (\(k,v) -> concat [k, "=\"", v, "\""]) attrs

showParser :: Parser String
showParser = 
    list_parser <|>
    tuple_parser <|>
    try record_parser <|>
    adt_parser <|>
    number <|>
    quoted_string <?> "Parse error"

quoted_string :: Text.Parsec.ParsecT String u Data.Functor.Identity.Identity [Char]
quoted_string = do
    s <- stringLiteral
    return $ "\"" ++ s ++ "\""

number = do
    n <- integer
    return $ show n

list_parser = do
    ls <- brackets $ commaSep showParser
    return $ tag "list" $ unwords $ map (tag "list-elt") ls
    
tuple_parser = do
    ls <- parens $ commaSep showParser
    return $ tag "tuple" $ unwords $ map (tag "tuple-elt") ls

record_parser = do
    ti <- type_identifier
    ls <- braces $ commaSep kvparser
    return $ tagAttrs "record" [("name", ti)] (unlines ls)

adt_parser = do
    ti <- type_identifier
    return $ tag "adt" ti

kvparser = do
    k <- identifier
    symbol "="
    t <- showParser
    return $ tagAttrs "elt" [("key", k)] t

type_identifier = do
    fst <- oneOf ['A'..'Z']
    rest <- many alphaNum
    whiteSpace
    return $ fst:rest

lexer         = P.makeTokenParser emptyDef
parens        = P.parens lexer
brackets      = P.brackets lexer
braces        = P.braces lexer
commaSep      = P.commaSep lexer
whiteSpace    = P.whiteSpace lexer
symbol        = P.symbol lexer
identifier    = P.identifier lexer
integer       = P.integer lexer
stringLiteral = P.stringLiteral lexer 