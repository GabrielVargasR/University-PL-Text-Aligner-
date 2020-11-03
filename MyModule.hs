module MyModule(
    Line,
    Token,
    lineify,
    stringify,
    tokenLength,
    lineLength,
    test
)
where
-- Data types
    type Line = [Token]
    data Token = Word String | Blank | HypWord String 
                deriving(Eq, Show)

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
    pruneLine line = dropWhile (== Blank) $ reverse $ dropWhile (==Blank) (reverse myLine)

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


-- for tests
    myLine = (Blank) : (Blank) : (Word "Aquel") : (Word "que") : (Blank) :(HypWord "contro") : (Word "la") : (Blank) : (Blank) : []
    test = stringify myLine