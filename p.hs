{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Char
import qualified Data.Text.Lazy as T
import Control.Applicative 
import Data.Attoparsec.Text.Lazy
import Data.Attoparsec.Combinator

tst1 = do
	s <- string "aa"
	skipMany1 $ char ' '
	return s

tst2 = do
	s <- string "bb" <|> string "cc"
	skipMany1 $ char ' '
	return s

search p end = match scan
	where scan = (end) <|> (p >> scan)
