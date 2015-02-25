module Times 
(
	DudeDate(..)
	, EventTime(..)
)
where

import Task
import Control.Applicative
import Control.Monad.Reader
import Data.Time.Calendar
import Data.Time.LocalTime

data DueDate d t = DueDate {dueDate :: TimePair d t} deriving (Eq,Ord)

instance (Show t) => Timeable (DueDate d t) where
	showFirstStartTime _ = ""
	showSecondStartTime = show . time . dueDate
	toMinutes _ = 0

data EventTime = EventTime
	{ startDate :: TimePair Day TimeOfDay
	, endDate :: TimePair Day TimeOfDay
	}

instance Timeable EventTime where
	showFirstStartTime = take 5 . show . time . startDate
	showSecondStartTime = take 5 . show . time . endDate

	toMinutes (EventTime {startDate=sd,endDate=ed}) = tm + th + (fromInteger td)
		where
			td = ((date ed) `diffDays` (date sd)) * 1440
			th = ((todHour $ time ed) - (todHour $ time sd)) * 60
			tm = (todMin $ time ed) - (todMin $ time sd)

-- testing stuffs
tst_dt = fromGregorian 2015 02 10
tst_t1 = TimeOfDay 12 30 0
tst_t2 = TimeOfDay 17 45 0
tst_tm = DueDate $ TimePair (tst_dt,tst_t1)
tst_td = EventTime (TimePair (tst_dt,tst_t1)) (TimePair (tst_dt,tst_t2))
tsk = Task tst_tm 'A' "blarg nad fa;ksdjf ;ajsd"
evnt = Task tst_td 'B' "b;laksdjf s;dlkfj ;asjpowier;askdjf ;jasdp"

tst a = runReader (showTask a) newConfig

newConfig = defaultConfig {minutesperline = 60}
