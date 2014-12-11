{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Char
import qualified Data.Text.Lazy as T
import Control.Applicative 
import Data.Attoparsec.Text.Lazy
import Data.Attoparsec.Combinator

runTst = maybeResult . parse tst

tst = do
	t1 <- tst1
	t2 <- option ("") tst2
	return (t1,t2)

tst1 = do
	s <- string "aa"
	skipMany1 $ char ' '
	return s

tst2 = do
	s <- string "bb" <|> string "cc"
	skipMany1 $ char ' '
	return s
