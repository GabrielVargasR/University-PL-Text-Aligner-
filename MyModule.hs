module MyModule(
    Line,
    Token,
    lineify,
    stringify,
    tokenLength,
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

    stringify :: Line -> String
    stringify line = init $ concat $ map tokenString line
    -- Falta tomar en cuenta 

-- Calculates the length of a Token
    tokenLength :: Token -> Int
    tokenLength (Blank) = 1
    tokenLength (Word tok) = length tok
    tokenLength (HypWord tok) = length tok + 1



    test = tokenLength Blank