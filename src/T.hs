{-# LANGUAGE MultiParamTypeClasses #-}

import qualified Data.List as L
import Control.Applicative
import Data.Time.LocalTime as LT
import Data.Time.Calendar as C

colwidth = 20
linechar = '-'
colchar = '|'
minutesperline = 30
pTimeLength = colwidth-6
pPrioLength = 3
sndTimePos = colwidth-12

class (Show t) => Timeable t where
	-- must be able to compute the # of minutes
	toMinutes :: (Integral y) => t -> y

-- TODO: refine these type classes to set up the structure for printing a task
class Taskable t where
	showTask :: (Timeable tt, Show tt, Show p, Show d) => t tt p d -> String

-- a task is made of a start date, end date, description, and a priority
-- taskTimes should be of at least length 2
data Task tt p d = Task
	{taskTime :: tt
	,taskPrio :: p 
	,taskDesc :: d
	}	deriving (Eq, Ord, Show)

{-f :: (Timeable t, Show d) => t -> d -> String-}
{-f t = showDesc (diffTime <*> timeNull $ t) . show -}

instance Taskable Task where
	showTask (Task {taskTime=tm,taskPrio=p,taskDesc=d}) = 
			(pt tm p) -- print the line with priority and time info
			++ ddblk -- print the description (formatted)
			++ lf -- put a blank line at the end
		where 
			pt t = pTimeLine (show t) . show
			dd t = showDesc (toMinutes t) . show
			dblk = dd tm d
			ddblk -- insert a new line for the description block if it's not empty
				| (length dblk > 0) = '\n':dblk
				| otherwise = ""
			lf -- a filled line for the bottom of a task (doesn't show if there is no description block)
				| (length dblk > 0) = lineFilled
				| otherwise = ""

data OnceTime d t = OnceTime d t deriving (Eq,Ord)

instance (Show d, Show t) => Show (OnceTime d t) where
	show (OnceTime d t) = pSecondTime t

instance (Show d, Show t) => Timeable (OnceTime d t) where
	toMinutes (OnceTime _ _) = 0

-- takes the time format string and the priority for a task and creates a filled
-- line 
pTimeLine ::  String -> String -> String
pTimeLine t p = insertAt 7 (take pTimeLength t)
	$ insertAt 3 p lineFilled

-- left and right spots on the time line
-- (helper functions for showing Timeable types)
pFirstTime t = insertAt 0 (take 5 $ show t) ptLine
pSecondTime t = insertAt sndTimePos (take 5 $ show t) ptLine
ptLine = replicate pTimeLength linechar

-- format the description given the time difference and the string
showDesc :: (Real t) => t -> String -> String
showDesc t d = truncLines $ (L.intercalate "\n" $ putInLine <$> formatLines) ++ blines 
	where
		formatLines = splitEvery colwidth d
		blines = ('\n':) . nBlankLines $ n 
		n = (timesToLines t) - (length formatLines)
		truncLines = unlines . (\s -> take (sn s) s) . lines 
		sn = (n+) . length

-- converts a time difference (in minutes) into a number of lines
timesToLines :: (Real n) => n -> Int
timesToLines = round . (/minutesperline) . toRational

-- convenience functions to draw lines with column markers
-- a : |--...--| line
lineFilled = line linechar

-- a : |  ...  | line
lineBlank = line ' '

nBlankLines :: Int -> String
nBlankLines x
	| x <= 0 = ""
	| otherwise = L.intercalate "\n" $ L.replicate x lineBlank

putInLine = (flip $ insertAt 1) lineBlank
-- a : |cc...cc| line
--
line c = colchar : (replicate colwidth c) ++ (colchar:[])

-- generic insert at function (used above to insert text into a line 
-- NOTE: if length needle > length haystack then the needle will be truncated to
-- fit within the line
insertAt :: Int -> [a] -> [a] -> [a]
insertAt index needle haystack = take (length haystack) $ prefix ++ needle ++ suffix
	where 
		prefix = take index haystack
		suffix = drop ns haystack
		ns = (index+) $ length needle

-- takes a list and splits it into a list of lists each with a length of at
-- least n
-- used above to format the description
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = (fst sp) : splitEvery n (snd sp)
	where sp = L.splitAt n xs

dt = fromGregorian 2015 02 10
tt = TimeOfDay 12 30 0
tm = OnceTime dt tt
tsk = Task tm 'A' "blarg nad fa;ksdjf ;ajsd"
