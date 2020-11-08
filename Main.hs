import Prelude hiding (null, lookup, map, filter)
import Data.Map.Lazy hiding (sort,map,foldl)
import Data.Char
import Data.List (sort,map)
import System.IO
import Aligners

main :: IO()
main = do 
        mainloop (fromList [])

mainloop :: HypMap -> IO()
mainloop estado = do
    putStrLn ">>"
    inp <- getLine
    let tokens = words inp
    let command = head tokens
    
    case command of
        "load" -> do
            inh <- openFile (last tokens) ReadMode
            nuevoestado <- loadDict inh estado
            hClose inh
            putStrLn $ "Diccionario cargado (" ++ (show $ size nuevoestado) ++ " palabras)"
            mainloop nuevoestado
        "show" -> do
            putStrLn $ show estado
            mainloop estado
        "ins" -> do
            let (palabra, silabas) = (\arr->(head arr, last arr)) (tail tokens)
            let nuevoestado = insert palabra (syllables silabas) estado
            putStrLn $ "Palabra " ++ palabra ++ " agregada"
            mainloop nuevoestado
        "save" -> do
            outh <- openFile (last tokens) WriteMode
            saveDict outh (sort (toList estado))
            hClose outh
            putStrLn $ "Diccionario guardado (" ++ (show $ size estado) ++ " palabras)"
            mainloop estado
        "split" -> do
            putStrLn "How's everything going"
            mainloop estado
        "splitf" -> do
            putStrLn "How's everything going"
            mainloop estado
        "exit" -> do
            putStrLn "Saliendo..."
        _ -> do
            putStrLn "Comando desconocido. Intente otra vez"
            mainloop estado

syllables :: String -> [String]
syllables word = let
                    (arr, rem) = foldl (\(fin, con) c -> if c /= '-' then (fin, con ++ [c]) else (fin++[con], "")) ([],"") word
                 in arr ++ [rem]

loadDict :: Handle -> HypMap -> IO HypMap
loadDict handle estado = do
                isEOF <- hIsEOF handle
                if isEOF then return estado
                else do 
                    def <- hGetLine handle
                    let keyVal = words def
                    let nuevoestado = insert (head keyVal) (syllables $ last keyVal) estado
                    loadDict handle nuevoestado

makeSyllables :: [String]->String
makeSyllables arr = init $ foldl (\str syl-> str ++ syl ++ "-") "" arr

saveDict :: Handle -> [(String, [String])] -> IO ()
saveDict outh [] = return ()
saveDict outh ((key,val):pairs) = do 
                                        hPutStrLn outh $ key ++ " " ++ (makeSyllables val)
                                        saveDict outh pairs