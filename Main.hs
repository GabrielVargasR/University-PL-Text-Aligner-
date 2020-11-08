import Prelude hiding (null, lookup, map, filter)
import Data.Map.Lazy hiding (sort,map,foldl)
import Data.Char
import Data.List (sort,map)
import System.IO
import Aligners

main = do
    putStrLn "Hi!"
    saludo <- getLine
    
    case saludo of
        "Hello!" -> do
            putStrLn "How's everything going"
        _ -> do
            putStrLn "Wrong answer"
            main
