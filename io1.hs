import Data.Char

main = do
     putStrLn "Whats your first name?"
     firstName <- getLine
     putStrLn "What's your last name?"
     lastName <- getLine
     let bigFirstName = map toUpper firstName
         bigLastName = map toUpper lastName
     putStrLn $ "Hey " ++ bigFirstName ++ " your last name is " ++ bigLastName
