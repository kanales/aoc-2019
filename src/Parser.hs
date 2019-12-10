module Parser
    ( digit
    , integer
    , uppercase
    , comma
    , commaSep
    , oneOf
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
integer =
    let ds  = munch1 isDigit
        ds' = (:) <$> (char '-') <*> ds
    in  read <$> (ds <|> ds')

uppercase :: ReadP Char
uppercase = satisfy (\c -> 'A' <= c && c <= 'Z')

comma :: ReadP Char
comma = char ','

commaSep :: ReadP a -> ReadP [a]
commaSep c = sepBy c comma

oneOf :: [a] -> ReadP a
oneOf []       = empty
oneOf (c : cs) = pure c <|> oneOf cs
