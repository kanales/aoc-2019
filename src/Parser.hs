module Parser where

import           Data.Char
import           Control.Applicative
import           Text.ParserCombinators.ReadP

digit :: ReadP Char
digit = satisfy isDigit

integer :: ReadP Int
integer = read <$> some digit
