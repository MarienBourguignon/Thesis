module Lexer where

import Intelx86
import Numeric
import Data.Char (toLower, toUpper)
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Char
import Text.Parsec.String ( Parser )
import qualified Text.Parsec.Token as Token


-- Defines the language to be parsed inside a record
languageDef = emptyDef { Token.commentStart    = "/*"
                       , Token.commentEnd      = "*/"
                       , Token.commentLine     = ";"
                       , Token.nestedComments  = False
                       , Token.opStart         = letter
                       , Token.opLetter        = letter
                       , Token.caseSensitive   = False
                       }
                       
-- Generates a lexer from the definition of the language
lexer = Token.makeTokenParser languageDef

-- Renaming of useful functions provided by the library
identifier = Token.identifier lexer
whiteSpace = Token.whiteSpace lexer
integer    = Token.integer    lexer
reserved   = Token.reserved   lexer
operator   = Token.operator   lexer

-- Non case sensitive char and string parsers
myChar :: Char -> Parser Char
myChar c = do result <- char (toLower c) <|> char (toUpper c)
              return . toLower $ result

myString :: String -> Parser String
myString s = mapM myChar s

-- Parse one of many strings
oneString :: [String] -> Parser String
oneString (x:[]) = myString x
oneString (x:xs) = try (myString x) <|> oneString xs

-- Hexadecimal parser
hexadecimal :: Parser Integer
hexadecimal = do optional trailing
                 result <- many hexDigit
                 return $ fst (readHex result !! 0)
                 where trailing = do try (myString "0x")
                                     return ()

-- Parse a register
register = oneString registers
           where registers = map show (enumFrom (toEnum 0 :: Register))

-- Parser for the size indication
sizeSpec :: Parser String
sizeSpec = do result <- myString "word" <|> myString "byte" <|> myString "dword"
              whiteSpace >> myString "ptr" >> whiteSpace
              return result
          
oneLine str = 
    case parse register "(unknown)" str of
         Left e -> error $ show e
         Right r -> r 
          

-- It can either be a register, an immediate or a memory location


-- Details:
-- type Parser = Parsec String () 
-- type Parsec s u = ParsecT s u Identity
-- data ParsecT s u m a
-- where s is the stream type,
-- u is the user state type
-- m is the underlying monad with return type a