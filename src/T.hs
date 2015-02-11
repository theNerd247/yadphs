import qualified Data.List as L
import Control.Applicative

colwidth = 20
linechar = '-'
colchar = '|'

-- a task is made of a start date, end date, description, and a priority
-- taskTimes should be of at least length 2
data Task dt t p d = Task
	{taskTimes :: [TaskTime dt t]
	,taskPrio :: p
	,taskDesc :: d
	} deriving (Eq, Ord, Show)

data TaskTime d t = TaskTime
	{taskTimeDate :: d
	,taskTimeTime :: t
	} deriving (Eq, Ord, Show)

-- creates the display string for all of the TaskTime Pairs for a given 
-- Task. Returns the list of display strings
showTask :: (Show dt, Show t, Show p) => (Task dt t p String) -> [String]
showTask (Task {taskTimes=tts,taskPrio=p,taskDesc=d}) = pTasks (tts,p,d)
	where 
		pTasks ([],_,_) = [""]
		pTasks ([x],_,_) = [""]
		pTasks ((t1:t2:tt),p,d) = (pTask t1 t2 p d) : pTasks (tt,p,d)

		-- the print layout for a single task 
		pTask t1 t2 p d = (pTimeLine t1 t2 p)  -- print the line with priority and time info
			++ "\n" ++ (showDesc t1 t2 d)  -- print the description (formatted)
			++ "\n" ++ lineFilled -- put a blank line at the end

		pTimeLine t1 t2 p = insertAt 7 (show $ taskTimeTime t1)
			$ insertAt (colwidth-6) (show $ taskTimeTime t2)
			$ insertAt 3 (show p) lineFilled

-- formats the description of a task given the start and end times and the description
showDesc :: (Show ta) => ta -> ta -> String -> String
showDesc t1 t2 d = (L.intercalate "\n" $ putInLine <$> formatLines) ++ "\n" ++ blines 
	where
		formatLines = splitEvery colwidth d
		blines = nBlankLines $ (timesToLines t2 t1) - (length formatLines) 
		putInLine = (flip $ insertAt 1) lineBlank

-- converts a time difference into a number of lines
timesToLines :: a -> a -> Int
timesToLines t1 t2 = 5

-- convenience functions to draw lines with column markers
-- a : |--...--| line
lineFilled = line linechar

-- a : |  ...  | line
lineBlank = line ' '

nBlankLines = L.intercalate "\n" . (flip L.replicate) lineBlank

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

tst :: (Show dt, Show t, Show p) => (Task dt t p String) -> IO ()
tst = putStrLn . f 
	where f = (flip (!!) 0) . showTask
