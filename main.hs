data Volunteer = Volunteer { name :: String
                           , available :: [(Int, String)]
                           , qnt :: Int
                           } deriving (Show)

data Schedule = Schedule   { day :: Int
                           , period :: String
                           , vol1 :: String
                           , vol2 :: String
                           } deriving (Show)
    
getName :: Volunteer -> String
getName ( Volunteer {name = n, available = avail, qnt = q } ) = n

getVolunteersNames :: [ Volunteer ] -> [ String ] -> [ String ]
getVolunteersNames [] string_list = string_list
getVolunteersNames (x:xs) string_list = getVolunteersNames xs (string_list ++ [ getName x ] ) 

isAvailable :: [(Int, String)] ->(Int, String) -> Bool
isAvailable [] _ = False
isAvailable ((dw, p):xs) (dayOfWeek, period) = if (dw == dayOfWeek && p == period)
                                                    then True
                                                    else isAvailable xs (dayOfWeek, period) 

selectVolunteer :: [ Volunteer ] -> (Int, String) -> String
selectVolunteer [] _ = []
selectVolunteer (( Volunteer {name = n, available = avail, qnt = q } ):xs) period
        | ( isAvailable avail period ) = n
        | otherwise = selectVolunteer xs period  

{--
makeSchedule :: [ Volunteer ] -> Int -> [ Schedule ] -> [ Schedule ]
makeSchedule [] firstDay sch = sch
makeSchedule (x:xs) firstDay sch =  firstDay
--}


test :: [ Volunteer ]
test = [ Volunteer { name = "ABC", available = [ (2, "M"), (2, "T"), (3, "M"), (3, "T") ], qnt = 4 }
       , Volunteer { name = "DEF", available = [ (2, "M"), (2, "T"), (3, "M"), (3, "T") ], qnt = 2 } ]

