import System.Environment
import System.IO
import System.IO.Error
import Control.Exception

main = catch toTry handler

toTry :: IO ()
toTry = do  (fileName:_) <- getArgs
            contents <- readFile fileName
            putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e =
        case ioeGetFileName e of
            Just path -> putStrLn $ path ++ " does not exist"
            Nothing -> putStrLn "unknown file does not exist"
    | otherwise = ioError e