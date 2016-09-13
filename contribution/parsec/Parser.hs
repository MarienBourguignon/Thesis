module Parser where

import Lexer
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String ( Parser )




parseString str = 
    case parse testing "(unknown)" str of
         Left e -> error $ show e
         Right r -> r

         
