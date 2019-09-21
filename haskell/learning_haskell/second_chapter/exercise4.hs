data Client = GovOrg String
            | Company String Integer Person
	    | Personal Person Bool
	    deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

data Machine = TimeMachine Manufacturer Model String TravelOption Float
             deriving Show

data Manufacturer = Manufacturer String
                  deriving Show

data Model = Integer
           deriving Show

data TravelOption = ToFuture | ToPast | None
                  deriving Show

