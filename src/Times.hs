data DueDate d t = DueDate {dueDate :: TimePair d t} deriving (Eq,Ord)

instance (Show d, Show t) => Show (DueDate d t) where
	show = pSecondTime . time . dueDate

instance (Show d, Show t) => Timeable (DueDate d t) where
	toMinutes _ = 0

data EventTime d t = EventTime
	{ startDate :: TimePair d t
	, endDate :: TimePair d t
	}

{-TODO: see if there is a composure for pFirstTime and pSecondTime-}
instance (Show d, Show t) => Show (EventTime d t) where
	show = pFirstTime . time . startDate <*> pSecondTime . time . endDate 

instance Timeable (EventTime d t) where
	toMinutes (EventTime {startDate=sd,endDate=ed}) = dt + tt
		where
			dt = (date ed) - (date sd) * 1440


