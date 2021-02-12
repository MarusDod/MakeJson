{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Parser where

    import qualified Data.Char as C
    import qualified Data.Text.Lazy as T
    import qualified Control.Monad.State as S
    import Data.Foldable(asum)
    import Control.Monad(void)

    import Control.Applicative(many,empty,(<|>),some,Alternative(..))

    data Code = Code {
        line :: Integer,
        file :: String,
        text :: T.Text
    } deriving Show

    data Perhaps e a = Nope | Err e | Ok a deriving (Show)

    newtype Parser a = Parser {
        getParser :: S.StateT Code (Perhaps Code) a
    } deriving (Functor,Applicative,Monad)

    instance Functor (Perhaps e) where
        fmap f Nope = Nope
        fmap f (Err e) = Err e
        fmap f (Ok a) = Ok (f a) 

    instance Applicative (Perhaps e) where
        pure x = Ok x
        Nope <*> _ = Nope
        (Err e) <*> _ = Err e
        (Ok f) <*> Ok a = Ok (f a) 
        (Ok f) <*> Nope = Nope
        (Ok f) <*> (Err e) = Err e

    instance Monad (Perhaps e) where
        return = pure
        Nope >>= _ = Nope
        (Err e) >>= _ = Err e
        (Ok a) >>= f = (f a) 

    instance Alternative (Perhaps Code) where
        empty = Nope 

        Nope <|> x = x
        Ok a <|> _ = Ok a
        Err e <|> _ = Err e

    instance Alternative Parser where
        empty = parser $ \_ -> Nope
        x <|> y = parser $ \s -> runParser x s <|> runParser y s


    failParser :: T.Text -> Parser a
    failParser str = parser $ \s -> Err $ s {text = str}

    runParser :: Parser a -> Code -> Perhaps Code (a, Code)
    runParser = S.runStateT . getParser

    parser = Parser . S.StateT

    parse :: Parser a -> String -> Perhaps Code (a, Code)
    parse x str = runParser x $ Code 0 "parser.hs" (T.pack str)

    parseVowels :: Parser Char
    parseVowels = parser $ \s -> let c = T.head $ text s
                                 in if c `elem` ['a'..'u']
                                        then Ok (c,s {text = T.tail $ text s});
                                        else runParser (failParser $ T.singleton c <> " is not a vowel") s

    char :: Char -> Parser Char
    char c = parser $ \s -> case runParser get s of
                                        Ok (x,xs) | x == c -> Ok (c,xs);
                                        Ok (x,xs) -> runParser (failParser $ "expected '" <> T.singleton c <> "' got " <> T.singleton x) s
                                        Err x -> Err x
                                        Nope -> Nope

    parseString :: T.Text -> Parser T.Text 
    parseString = fmap T.pack . mapM (optional . char) . T.unpack

    choice :: [Parser a] -> Parser a
    choice [] = empty
    choice xs = asum xs

    satisfy :: (Char -> Bool) -> Parser Char
    satisfy f = parser $ \s -> if "" == text s
                                    then runParser (failParser $ "unexpected eof") s
                                    else 
                                        if f (T.head $ text s)
                                            then Ok (T.head $ text s,s {text = T.tail $ text s})
                                            else runParser (failParser $ T.singleton (T.head $ text s) <> "did not satisfy predicate") s

    space :: Parser ()
    space = void $ many $
            (optional (parseString (T.pack "//")) >> (manyTill (optional get) parseNewline) >> return ' ')
                <|>
                        (optional (parseString (T.pack "/*")) >> manyTill (optional parseNewline <|> get) (parseString "*/") >> return ' ')
                            <|>
                                optional parseNewline
                                  <|>
                                      (optional $ choice $ map (optional . char) " \t")
            where parseNewline = parser $ \s -> case runParser (char '\n') s of
                                                    Err x -> Err x
                                                    Ok (x,xs) -> Ok (x,xs {line = (+1) $ line xs})
                                                    Nope -> Nope

    get :: Parser Char
    get = parser $ \s -> if T.null (text s)
                            then runParser (failParser "eof") s
                            else Ok (T.head $ text s,s {text = T.tail $ text s})

    notParser :: Parser a -> Parser ()
    notParser x = parser $ \s -> case runParser x s of
                                (Ok _) -> Err s
                                Err _ -> Ok ((),s)
                                Nope -> Nope


    optional :: Parser a -> Parser a
    optional x = parser $ \s -> case runParser x s of
                                    Err _ -> Nope
                                    Ok (a,q) -> Ok (a,q)
                                    Nope -> Nope

    option :: a -> Parser a -> Parser a
    option y x = parser $ \s -> case runParser x s of
                                    Err _ -> Ok (y,s)
                                    Ok (a,q) -> Ok (a,q)
                                    Nope -> Ok (y,s)

    surround :: Parser b -> Parser a -> Parser a
    surround sep x = sep *> x <* sep

    surroundSpaces :: Parser a -> Parser a
    surroundSpaces x = space *> x <* space


    sepBy :: Parser a -> Parser b -> Parser [a]
    sepBy x sep = many (optional $ space *> sep *> x)

    endBy :: Parser a -> Parser b -> Parser [a]
    endBy x sep = many (optional $ x <* sep)

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
        (optional end >> return []) <|> ((:) <$> x <*> manyTill x end)

    nullify :: Parser T.Text -> Parser T.Text
    nullify x = parser $ \s -> 
        case runParser x s of
            Err x -> Err x
            Ok (w,_) | w == T.empty -> runParser (failParser $ "did not expect " <> w) s
            Ok (a,q) -> Ok (a,q)
            Nope -> Nope

    nonEmpty :: Parser [a] -> Parser [a]
    nonEmpty p = parser $ \s ->
        case runParser p s of
            Err x -> Err x
            (Ok ([],_)) -> runParser (failParser "invalid empty string") s
            a -> a


    look :: Parser T.Text
    look = parser $ \s -> Ok (text s,s)

    peof :: Parser ()
    peof = parser $ \s -> if T.null (text s)
                            then Ok ((),s{text = T.empty})
                            else runParser (failParser "expected eof here") s

