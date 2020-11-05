module MyModule(
    Line,
    Token,
    HypMap,
    lineify,
    stringify,
    tokenLength,
    lineLength,
    breakLine,
    mergers,
    hyphenate,
    test
)
where
    import Prelude hiding (null, lookup, map, filter)
    import Data.Map.Lazy hiding (sort,map,foldl,take,drop)
    import Data.List (sort,map)
    import Data.Maybe
-- Data types
    type Line = [Token]
    data Token = Word String | Blank | HypWord String 
                deriving(Eq, Show)

    type HypMap = Map String [String]

-- Separates a String into a Line
    lineify :: String -> Line
    lineify "" = []
    lineify text = map (\w->Word w) (words text)

-- Converts a Line into a String (stringify)
    tokenString :: Token -> String
    tokenString (Blank) = " "
    tokenString (HypWord tok) = tok ++ "- "
    tokenString (Word tok) = tok ++ " "

    pruneLine :: Line -> Line
    pruneLine line = dropWhile (== Blank) $ reverse $ dropWhile (==Blank) (reverse line)

    stringify :: Line -> String
    stringify line = init $ concat $ map tokenString (pruneLine line)

-- Calculates the length of a Token
    tokenLength :: Token -> Int
    tokenLength (Blank) = 1
    tokenLength (Word tok) = length tok
    tokenLength (HypWord tok) = length tok + 1

-- Calculates the length of a line
    lineLength :: Line -> Int
    lineLength line = length $ stringify line

-- Breaks a Line in two, such that the first part does not exceed a given limit
    midLength :: Int->Token->Token->Int
    midLength count (Blank) incoming = count + (tokenLength incoming)
    midLength count _ incoming = count + (tokenLength incoming) + 1

    lengthCounter :: (Int, Int, Line, Line) -> Token -> (Int, Int, Line, Line)
    lengthCounter (lim, count, fst, sec) tok = if (midLength count (last fst) tok) <= lim then (lim, midLength count (last fst) tok, fst++[tok], sec)
                                               else (lim, lim, fst, sec++[tok])

    breakLine :: Int -> Line -> (Line, Line)
    breakLine limit line = let 
                                (_,_,fst, sec) = foldl (lengthCounter) (limit, 0, [Blank],[]) (pruneLine line)
                           in (tail fst, sec)

-- Generates all possible ways to concatenate a string provided as a list of syllables into two separated syllables
    mergers :: [String]->[(String, String)]
    mergers syllables = [(concat $ take s syllables, concat $ drop s syllables) | s <- [1..length syllables-1]]

-- Generates all possible ways to concatenate a Word in two parts, using HypWord and Word data
    hyphenate :: HypMap -> Token -> [(Token,Token)]
    hyphenate hmap (Word str) = map (\(a,b)->((HypWord a),(Word (b++punct)))) (mergers $ fromJust $ lookup punct' hmap)
                                where punct = (dropWhile (\n->notElem n ['.', ',', '!', '?','"']) str)
                                      punct' = (takeWhile (\n->notElem n ['.', ',', '!', '?','"']) str)


-- for tests
    myLine = (Blank) : (Blank) : (Word "Aquel") : (Word "que") : (Blank) :(HypWord "contro") : (Word "la") : (Blank) : (Blank) : []
    sepWord = ["con", "tro", "la"]
    enHyp :: HypMap
    enHyp = fromList [("controla", ["con", "tro", "la"]), ("futuro", ["fu", "tu", "ro"]), ("presente", ["pre", "sen", "te"])]

    test = hyphenate enHyp (Word "futuro.,.!")
    
