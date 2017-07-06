module HEP.Data.ParserUtil (skipTillEnd) where

import Data.Attoparsec.ByteString       (skipWhile)
import Data.Attoparsec.ByteString.Char8 hiding (skipWhile)

skipTillEnd :: Parser ()
skipTillEnd = skipWhile (not . isEndOfLine)
