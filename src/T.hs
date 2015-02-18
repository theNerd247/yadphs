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

-- | Anything that can represent the time structure of a task. This can include
-- anything (like due dates, event start and end times, recuring due dates, etc.)
class (Show t) => Timeable t where
	-- | the amount of time the Timeable covers. For a task that just has a due
	-- date this will be 0 for an event it will be the length of the event (in
	-- minutes)
	toMinutes :: (Integral y) => t -> y

-- | a Taskable type must have: a Timeable structure, a priority, and a
-- description (all of which much be showable). This class is the main structure
-- for managing the behavior of tasks throughout the program 
class Taskable t where
	-- | extracts the Timeable from the task
	getTime :: (t tt p d) -> tt
	-- | extracts the priority from the task
	getPrio :: (t tt p d) -> p
	-- | extracts the description from the task
	getDesc :: (t tt p d) -> d
	-- | shows the task in block format (as shown below). The height of the block is
	-- determined by the number of lines the description spans and the end time of
	-- the task. If toMinutes of the Timeable part of the task evaluates to 0 then
	-- the task will be printed as a single line. 
	-- 
	-- Example: (Taskable T) => showTask T
	--
	-- > |-(A)-10:30-----01:45-|
	-- > |Lorem ipsum dolor sit|
	-- > | amet, consetetur sad|
	-- > |ipscing elitr,       |
	-- > |                     |
	-- > |                     |
	-- > |                     |
	-- > |                     |
	-- > |---------------------|
	-- 
	showTask :: (Timeable tt, Show tt, Show p, Show d) => (t tt p d) -> String
	showTask t = 
			(pt tm p) -- print the line with priority and time info
			++ ddblk -- print the description (formatted)
			++ lf -- put a blank line at the end
		where 
			tm = getTime t
			p = getPrio t
			d = getDesc t
			pt t = pTimeLine (show t) . show
			dd t = formatDesc (toMinutes t) . show
			dblk = dd tm d
			ddblk -- insert a new line for the description block if it's not empty
				| (length dblk > 0) = '\n':dblk
				| otherwise = ""
			lf -- a filled line for the bottom of a task (doesn't show if there is no description block)
				| (length dblk > 0) = lineFilled
				| otherwise = ""

-- | The main task structure. No other structure should be needed unless the
-- you need a more specific way to implement the Timeable, priority, and/or
-- description (in which case use the newtype keyword).
data Task tt p d = Task
	{taskTime :: tt
	,taskPrio :: p 
	,taskDesc :: d
	}	deriving (Eq, Ord, Show)

instance Taskable Task where
	getTime = taskTime
	getPrio = taskPrio
	getDesc = taskDesc

data TimePair d t = TimePair 
	{ date :: d
	, time :: t
	} deriving (Eq,Ord)

data DueDate d t = DueDate {dueDate :: TimePair d t} deriving (Eq,Ord)

instance (Show d, Show t) => Show (DueDate d t) where
	show = pSecondTime . time . dueDate

instance (Show d, Show t) => Timeable (DueDate d t) where
	toMinutes _ = 0

data EventTime d t = EventTime
	{	startDate :: TimePair d t
	, endDate :: TimePair d t
	}

-- TODO: see if there is a composure for pFirstTime and pSecondTime
{-instance (Show d, Show t) => Show (EventTime d t) where-}
	{-show et = pFirstTime (time . startDate ) . pSecondTime . time . endDate $ et-}

-- takes the time format string and the priority for a task and creates a filled
-- line 
pTimeLine ::  String -> String -> String
pTimeLine t p = insertAt 7 (take pTimeLength t)
	$ insertAt 3 p lineFilled

-- left and right spots on the time line
-- (helper functions for showing Timeable types)
pFirstTime t = insertAt 0 (take 5 $ show t) 
pSecondTime t = insertAt sndTimePos (take 5 $ show t) ptLine
ptLine = replicate pTimeLength linechar

-- format the description given the time difference and the string
formatDesc :: (Real t) => t -> String -> String
formatDesc t d = truncLines $ (L.intercalate "\n" $ putInLine <$> formatLines) ++ blines 
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

{-dt = fromGregorian 2015 02 10-}
{-tt = TimeOfDay 12 30 0-}
{-tm = OnceTime dt tt-}
{-tsk = Task tm 'A' "blarg nad fa;ksdjf ;ajsd"-}
