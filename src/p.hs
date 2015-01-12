{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Char
import qualified Data.Text as T
import Control.Applicative 
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Text.Internal as TT

data B = Non | B {bl :: Int} deriving (Eq,Ord,Show)

tst = parseOnly 

t1 = do
	p <- priority
	d <- description
	eol
	return (p,d)

eol = char '\n' 
	<|> char '\r'

description = takeTill e 
	where e c = c == '\n' || c == '-'
	{-skipSpace-}
	{-return $ T.dropWhileEnd (=='\n') $ T.pack d --remove any trailing newline characters-}

-- parses the priority of our task
priority = do
	char '('
	prio <- satisfy $ inClass "A-Z"	
	char ')'
	return prio

sscan = scan "" w
	where 
		w s c = p $ c:s
		p "+-" = Nothing
		p xs = Just xs
