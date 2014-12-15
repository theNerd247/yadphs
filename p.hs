{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Char
import qualified Data.Text.Lazy as T
import Control.Applicative 
import Data.Attoparsec.Text.Lazy
import Data.Attoparsec.Combinator
import Data.Text.Internal as TT

tst1 = do
	s <- string "aa"
	skipMany1 $ char ' '
	return s

tst2 = do
	s <- string "bb" <|> string "cc"
	skipMany1 $ char ' '
	return s

tst s =  t2 <$> parse t1 s 

t1 = string "ab"

t2 s = parse (char 'b') s
