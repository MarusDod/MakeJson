module ParseSettingsJson(JSON(..),jsonParseFile) where

    import Parser
    import qualified Data.Text.Lazy as T
    import qualified Data.Text.Lazy.IO as T
    import Control.Applicative((<|>),many)

    data JSON 
        = JSARRAY [JSON]
        | JSSTRING T.Text 
        | JSVAR T.Text
        | JSOBJECT [(JSON,JSON)]
        deriving Show
    

    json,jsonArray,jsonVar,jsonObject,jsonString :: Parser JSON

    json = surroundSpaces $ choice [
            jsonArray,
            jsonVar,
            jsonString,
            jsonObject
        ]

    jsonArray = JSARRAY <$>
        betInter (optional $ char '[') (char ']') (char ',') json
            
    jsonString = JSSTRING <$>
        (optional (char '\"') >> T.pack <$> manyTill get (char '\"'))

    jsonVar = JSVAR <$>
        (surround (optional $ char '\"') $ surroundSpaces $
          (optional (char '$') *>
              (between (char '(') (char ')') $
                  surroundSpaces $
                      T.pack <$> (nonEmpty $ many $ optional $ satisfy (`elem` validChars)))))
          where validChars = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "-_:"

    jsonObject = JSOBJECT <$>
        (betInter (optional $ char '{') (char '}') (char ',') $
            ((,) <$> objParsers <*> json))
            where objParsers = (surroundSpaces $
                    jsonVar <|> jsonString)
                        <* (char ':')

    jsonParseFile :: String -> IO (Either String JSON)
    jsonParseFile filename = do
        x <- T.readFile filename
        case (runParser json $ Code 0 filename x) of
              Nope -> return $ Left "unable to parse"
              Err e -> return $ Left $ "Syntax Error:\n\t" <> "at file '" <> file e <> "'\n\t" <> "line " <> show (line e) <> "\n\t" <> T.unpack (text e)
              Ok (_,s) | text s /= T.empty -> return $ Left $ "leftover:\n\t" <> T.unpack (text s)
              Ok (js,s) -> return $ Right js




