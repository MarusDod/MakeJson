module Parserbackup(
    Parser,
    runParser,
    many,
    some,
    nullify,
    nonEmpty,
    parseString,
    surround,
    surroundSpaces,
    char,
    parser,
    choice,
    satisfy,
    betInter,
    space,
    get,
    look,
    optional,
    option,
    sepBy,
    endBy,
    peof,
    negate,
    intercalated,
    (<|>),
    between,
    manyTill,
) where

    import qualified Data.Char as C
    import qualified Data.Text.Lazy as T
    import qualified Control.Monad.State as S
    import Control.Monad(void)

    import Control.Applicative(many,empty,(<|>),some)

    type Parser a = S.StateT T.Text Maybe a

    runParser = S.runStateT
    parser = S.StateT 

    parseVowels :: Parser Char
    parseVowels = parser $ \s -> if T.head s `elem` "aeiou"
                                        then Just (T.head s,T.tail s);
                                        else Nothing
    

    char :: Char -> Parser Char
    char c = parser $ \s -> case runParser get s of
                                        Just (x,xs) | x == c -> Just (T.head s,T.tail s);
                                        _ -> Nothing

    parseString :: T.Text -> Parser T.Text 
    parseString = fmap T.pack . mapM char . T.unpack

    choice :: [Parser a] -> Parser a
    choice [] = empty
    choice xs = foldr (<|>) empty xs

    satisfy :: (Char -> Bool) -> Parser Char
    satisfy f = parser $ \s -> if T.empty == s
                                    then Nothing 
                                    else 
                                        if f (T.head s)
                                            then Just (T.head s,T.tail s)
                                            else Nothing

    space :: Parser ()
    space = void $ many $ choice $ 
        (parseString (T.pack "//") >> (manyTill get (char '\n')) >> return ' ')
                :
                    (parseString (T.pack "/*") >> manyTill get (parseString $ T.pack "*/") >> return ' ')
                        :
                            (map char " \n\t")
    
    get :: Parser Char
    get = parser $ \s -> if T.null s 
                            then Nothing 
                            else Just (T.head s,T.tail s)

    notParser :: Parser a -> Parser ()
    notParser x = parser $ \s -> case runParser x s of
                                (Just _) -> Nothing
                                Nothing -> Just ((),s)


    optional :: Parser a -> Parser ()
    optional x = parser $ \s -> case runParser x s of
                                    Nothing -> Just ((),s)
                                    Just (a,q) -> Just ((),q)

    option :: a -> Parser a -> Parser a
    option y x = parser $ \s -> case runParser x s of
                                    Nothing -> Just (y,s)
                                    Just (a,q) -> Just (a,q)

    surround :: Parser b -> Parser a -> Parser a
    surround sep x = sep *> x <* sep

    surroundSpaces :: Parser a -> Parser a
    surroundSpaces x = space *> x <* space


    sepBy :: Parser a -> Parser b -> Parser [a]
    sepBy x sep = many (space *> sep *> x)

    endBy :: Parser a -> Parser b -> Parser [a]
    endBy x sep = many (x <* sep)

    between :: Parser a -> Parser a -> Parser b -> Parser b
    between open close x = open *> x <* close

    intercalated :: Parser a -> Parser b -> Parser [a]
    intercalated x sep = ((:) <$> x <*> sepBy x sep)
                            <|> return []

    betInter :: Parser a -> Parser a -> Parser b -> Parser c -> Parser [c]
    betInter open close sep x =
        between open close $
            intercalated x sep

    manyTill :: Parser a -> Parser b -> Parser [a]
    manyTill x end = 
        (end >> return []) <|> ((:) <$> x <*> manyTill x end)

    nullify :: Parser T.Text -> Parser T.Text
    nullify x = parser $ \s -> 
        case runParser x s of
            Nothing -> Nothing
            Just (w,_) | w == T.empty -> Nothing
            Just (a,q) -> Just (a,q)

    nonEmpty :: Parser [a] -> Parser [a]
    nonEmpty p = parser $ \s ->
        case runParser p s of
            Nothing -> Nothing
            (Just ([],_)) -> Nothing
            a -> a


    look :: Parser T.Text
    look = parser $ \s -> Just (s,s)

    peof :: Parser ()
    peof = parser $ \s -> if T.null s
                            then Just((),T.empty)
                            else Nothing

