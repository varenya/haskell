import Data.Maybe

--safe Head Function!
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

--safe Tail function
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

--safe Last function
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x : []) = Just x
safeLast (x:xs) = safeLast xs

--safe Init function
safeInit :: [a] -> Maybe [a]
safeInit (x: []) = Just []
safeInit (x:xs) = Just ( x : fromMaybe [] (safeInit xs) )
safeInit [] = Nothing
