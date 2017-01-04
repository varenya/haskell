safeSecond :: [a] -> Maybe a

safeSecond [] = Nothing
safeSecond (x:xs) = if null xs
		    then Nothing 
		    else Just (head xs) 

tinySecond :: [a] -> Maybe a
tinySecond (_:x:_) = Just x
tinySecond _       = Nothing
