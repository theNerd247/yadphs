import Data.List
import Text.Parsec
import Text.Parsec.String
import Data.Functor.Identity

data ParsedInfo = ParsedInfo Char String ([String],[String])

instance Show ParsedInfo where
	show (ParsedInfo p d (t,dt)) = 
		showString (show p) $ 
		showString (d) $ 
		showString (foldl1 (++) $ intersperse "/" t) $
		foldl1 (++) $ intersperse ":" dt


line :: Parser ParsedInfo
line = do 
	p <- priority
	d <- description
	t <- eventInfo
	char '\n'
	return (ParsedInfo p d t)	

description :: Parser String
description = many (noneOf "--,\n") 

priority :: Parser Char
priority = between (char '(') (char ')') upper

eventInfo :: Parser ([String],[String])
eventInfo = do 
	string "--" 
	d <- twoNum "/"
	t <- twoNum ":"
	return (d,t)

-- all time/date data are a series of two digits and seperated by a character
twoNum :: String -> Parser [String]
twoNum s = sepBy1 (count 2 digit) (string s)
