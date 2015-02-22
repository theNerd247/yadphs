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
	} deriving (Show)

pTimeLine ::  String -> String -> Reader YadpConfig String
pTimeLine t p = do
	plen <- asks pTimeLength
	line <- lineFilled
	return 
		$ insertAt 7 (take plen t) 
		$ insertAt 3 p line

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
	, pPrioLength = 3
	, pTimeLength = 13
	, sndTimePos = 14
	}

pFirstTime t = return . insertAt 0 (take 5 $ show t) 

pSecondTime t = do
	pline <- ptLine
	sndtpos <- asks sndTimePos
	return $ insertAt sndtpos (take 5 $ show t) pline

ptLine :: Reader YadpConfig String
ptLine = do
	ptl <- asks pTimeLength
	lch <- asks linechar
	return $ L.replicate ptl lch 

-- format the description given the time difference and the string
formatDesc :: Int -> String -> Reader YadpConfig String
formatDesc t d = do
	nLines <- timesToLines t
	fLines <- formatLines nLines d
	blines <- appendLines nLines (length fLines)
	return $ (L.intercalate "\n" fLines) ++ blines 
	where 
		appendLines n f
			| n > f = ("\n"++) <$> (nBlankLines (n-f))
			| otherwise = return ""

formatLines :: Int -> String -> Reader YadpConfig [String]
formatLines nLines s = do
	cw <- asks colwidth
	sequence $ putInLine <$> splitEvery cw (take (nLines*cw) s)

-- TODO:: rewrite round function to estimate # lines better
timesToLines :: Int -> Reader YadpConfig Int
timesToLines n = (asks minutesperline) >>= \mpl -> return . round . (/(toRational mpl)) . toRational $ n

-- takes a list and splits it into a list of lists each with a length of at
-- least n
-- used above to format the description
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = (fst sp) : splitEvery n (snd sp)
	where sp = L.splitAt n xs


