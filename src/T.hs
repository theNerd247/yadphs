{-# LANGUAGE MultiParamTypeClasses #-}

import qualified Data.List as L
import Control.Applicative

colwidth = 20
linechar = '-'
colchar = '|'
minutesperline = 30

-- TODO: refine these type classes to set up the structure for printing a task
class (Timeable t, Functor tt) => Taskable tt t p d where
	showTask :: (tt t p d) -> String

class Timeable t where
	date :: (Show y, Integral y) => t -> y
	time :: (Show y, Integral y) => t -> y
	timesTolines :: (Integral y) => t -> t -> y
	formatDesc :: t -> t -> String -> String

-- a task is made of a start date, end date, description, and a priority
-- taskTimes should be of at least length 2
data Task tt p d = Task
	{taskTimes :: [tt]
	,taskPrio :: p 
	,taskDesc :: d
	}	deriving (Eq, Ord, Show)

data TaskTime d t = TaskTime
	{taskTimeDate :: d
	,taskTimeTime :: t
	} deriving (Eq, Ord, Show)

-- creates the display string for all of the TaskTime Pairs for a given 
-- Task. Returns the list of display strings
showTask ::  => (Task dt t p d) -> [String]
showTask (Task {taskTimes=tts,taskPrio=p,taskDesc=d}) = pTasks (tts,p,d)
	where 
		pTasks ([],_,_) = [""]
		pTasks ([x],_,_) = [""]
		pTasks ((t1:t2:tt),p,d) = (pTask t1 t2 p d) : pTasks (tt,p,d)

		-- the print layout for a single task 
		pTask t1 t2 p d = (pTimeLine t1 t2 p)  -- print the line with priority and time info
			++ "\n" ++ (showDesc t1 t2 d)  -- print the description (formatted)
			++ lineFilled -- put a blank line at the end

		pTimeLine t1 t2 p = insertAt 7 (show $ taskTimeTime t1)
			$ insertAt (colwidth-6) (show $ taskTimeTime t2)
			$ insertAt 3 (show p) lineFilled

-- formats the description of a task given the start and end times and the description
showDesc :: (Integral ta, Integral tb, Show ta, Show tb, Show d) => (TaskTime ta tb) -> (TaskTime ta tb) -> d -> String
showDesc t1 t2 d = truncLines $ (L.intercalate "\n" $ putInLine <$> formatLines) ++ blines 
	where
		formatLines = splitEvery colwidth (show d)
		blines = ('\n':) . nBlankLines $ n 
		putInLine = (flip $ insertAt 1) lineBlank
		n = fromIntegral (timesToLines t1 t2) - (length formatLines) 
		truncLines = unlines . (\s -> take (sn s) s) . lines 
		sn = (n+) . length

-- converts a time difference into a number of lines
timesToLines :: (Integral dt, Integral t, Integral y) => (TaskTime dt t) -> (TaskTime dt t) -> y
timesToLines (TaskTime dt1 t1) (TaskTime dt2 t2) = linesPerMinute $ (fromIntegral deldt)+(fromIntegral delt)
	where 
		deldt = dt2 - dt1
		delt = t2 - t1
		linesPerMinute = round . (/minutesperline) . toRational

-- convenience functions to draw lines with column markers
-- a : |--...--| line
lineFilled = line linechar

-- a : |  ...  | line
lineBlank = line ' '

nBlankLines :: Int -> String
nBlankLines x
	| x <= 0 = ""
	| otherwise = L.intercalate "\n" $ L.replicate x lineBlank

-- a : |cc...cc| line
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

ta = TaskTime 12 10
tb = TaskTime 182 10
t = Task [ta,tb,ta,tb] 'a' "Lorem ipsum dolor sit amet, consetetur sadipscing elitr,"
tst :: (Integral dt, Integral t, Show dt, Show t, Show p) => (Task dt t p String) -> IO ()
tst = f . showTask
	where 
		f [] = return ()
		f (s:ss) = putStrLn s >>= (\s -> f ss)
