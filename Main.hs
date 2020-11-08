import Prelude hiding (null, lookup, map, filter)
import Data.Map.Lazy hiding (sort,map,foldl)
import Data.Char
import Data.List (sort,map)
import System.IO
import Aligners

main :: IO()
main = do 
        mainloop

mainloop :: IO()
mainloop = do
    putStrLn ">>"
    inp <- getLine
    let tokens = words inp
    let command = head tokens
    
    case command of
        "load" -> do
            inh <- openFile (last tokens) ReadMode
            loadDict inh
            hClose inh
            -- putStrLn $ "Diccionario cargado (" ++"n" ++ " palabras)"
            mainloop
        "show" -> do
            putStrLn "How's everything going"
            mainloop
        "ins" -> do
            putStrLn "How's everything going"
            mainloop
        "save" -> do
            putStrLn "How's everything going"
            mainloop
        "split" -> do
            putStrLn "How's everything going"
            mainloop
        "splitf" -> do
            putStrLn "How's everything going"
            mainloop
        "exit" -> do
            putStrLn "Saliendo..."
        _ -> do
            putStrLn "Comando desconocido. Intente otra vez"
            mainloop

loadDict :: Handle -> IO ()
loadDict handle = do
                isEOF <- hIsEOF handle
                if isEOF then return ()
                else do 
                    def <- hGetLine handle
                    putStrLn $ head $ words def
                    loadDict handle