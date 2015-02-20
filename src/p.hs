{-# LANGUAGE OverloadedStrings #-}
import qualified Data.List as L
import Control.Applicative
import Control.Monad.Reader

data YadpConfig = YadpConfig
	{ colwidth :: Int
	, linechar :: Char
	, colchar :: Char 
	, minutesperline :: Int 
	, pTimeLength :: Int 
	, pPrioLength :: Int
	, sndTimePos :: Int
	}

insertAt :: Int -> [a] -> [a] -> [a]
insertAt index needle haystack = take (length haystack) $ prefix ++ needle ++ suffix
	where 
		prefix = take index haystack
		suffix = drop ns haystack
		ns = (index+) $ length needle

nBlankLines :: Int -> Reader YadpConfig String
nBlankLines x
	| x <= 0 = return ""
	| otherwise = lineBlank >>= (return . L.intercalate "\n" . L.replicate x)

-- convenience functions to draw lines with column markers
-- a : |--...--| line
lineFilled :: Reader YadpConfig String
lineFilled = (asks linechar) >>= line 

-- a : |  ...  | line
lineBlank :: Reader YadpConfig String
lineBlank = line ' '

-- inserts a string into a blank line
putInLine :: String -> Reader YadpConfig String
putInLine s = do 
	ln <- lineBlank
	width <- asks colwidth
	return $ insertAt 1 (L.take width s) ln

-- a : |cc...cc| line
--
line :: Char -> Reader YadpConfig String
line c = do 
	width <- asks colwidth
	cchar <- asks colchar
	return $ cchar : (replicate width c) ++ (cchar:[])

config = YadpConfig
	{ colwidth = 20
	, linechar = '-'
	, colchar = '|'
	, minutesperline = 30
	, pTimeLength = 0
	, pPrioLength = 0
	, sndTimePos = 0
	}
