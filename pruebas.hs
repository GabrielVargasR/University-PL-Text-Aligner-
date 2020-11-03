import Prelude hiding (null, lookup, map, filter)
import Data.Map.Lazy hiding (sort,map,foldl)
import Data.Char
import Data.List (sort,map)
import System.IO
import MyModule

main = do
    putStrLn "Qué pa?"
    saludo <- getLine
    
    case saludo of
        "qué me Disney" -> do
            putStrLn "To' Goofy"
        _ -> do
            putStrLn "Sta mamando compa"
            main