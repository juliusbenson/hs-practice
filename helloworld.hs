import Data.Char

main = do
    putStrLn "enter first name"
    firstName <- getLine
    putStrLn "enter last name"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn ("hello " ++ bigFirstName ++ " " ++ bigLastName ++ ". Nice to meet you")

