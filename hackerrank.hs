compareTriplets = do
    trip1 <- read $ words $ getLine :: [Int]
    trip2 <- read $ words $ getLine :: [Int]
    let points (x,y)
            | x > y     = (1,0)
            | x < y     = (0,1)
            | otherwise = (0,0)
    putStrLn $ sum $ map fst $ map (points) $ zip trip1 trip2
    putStrLn $ sum $ map snd $ map (points) $ zip trip1 trip2