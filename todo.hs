import Data.Binary (decode,encode)
import qualified Data.ByteString as Strict (readFile,writeFile)
import qualified Data.ByteString.Lazy as Lazy (fromStrict,toStrict)
import Data.List (delete,intercalate)
import System.Directory (doesFileExist,getHomeDirectory,setCurrentDirectory)
import System.Environment (getArgs)
type Todo = (String,String)
main :: IO ()
main = do getHomeDirectory >>= setCurrentDirectory; getArgs >>= handle
get :: IO [Todo]
get = do
    exist <- doesFileExist ".todo"
    if exist then do
        content <- Strict.readFile ".todo"
        return $ decode $ Lazy.fromStrict content
    else return []
put :: [Todo] -> IO ()
put dat = Strict.writeFile ".todo" $ Lazy.toStrict $ encode dat
format :: (String -> String -> String) -> [Todo] -> IO ()
format _ [] = return ()
format f ((x,y):s) = do putStrLn $ f x y; format f s
handle :: [String] -> IO ()
handle [] = putStrLn "missing command"
handle ((cmd:opts):args) = call cmd opts args
call :: Char -> [Char] -> [String] -> IO ()
call 'a' _ (x:y) = do
    s <- get
    if lookup x s == Nothing then put $ (x,intercalate " " y):s else putStrLn $ "existing " ++ x
call 'a' _ _ = putStrLn "missing arguments"
call 'd' _ (x:_) = do
    s <- get
    case lookup x s of
        Just y -> put $ delete (x,y) s
        Nothing -> putStrLn $ "tag " ++ x ++ " not found"
call 'd' _ _ = putStrLn "missing tag"
call 'l' o _ = get >>= (format $ if elem 'd' o then \x y -> x ++ " - " ++ y else \x _ -> x)
call 's' o (x:_) = do
    s <- get
    case lookup x s of
        Just y -> putStrLn $ x ++ " - " ++ y
        Nothing -> putStrLn $ "tag " ++ x ++ " not found"
call 's' _ _ = putStrLn "missing tag"
call _ _ _ = putStrLn "unknown command"