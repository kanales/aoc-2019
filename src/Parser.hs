module Parser
    ( digit
    , integer
    , uppercase
    , commaSep
    , module ReadP
    )
where

import           Data.Char
import           Control.Applicative
import           Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP  as ReadP

digit :: ReadP Char
digit = satisfy isDigit

integer :: ReadP Int
integer = read <$> munch1 isDigit

uppercase :: ReadP Char
uppercase = satisfy (\c -> 'A' <= c && c <= 'Z')

comma :: ReadP Char
comma = char ','

commaSep :: ReadP a -> ReadP [a]
commaSep c = sepBy c comma
