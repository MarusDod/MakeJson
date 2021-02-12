{-# LANGUAGE FlexibleContexts #-}

module LoadSettings where

    import ParseSettingsJson

    import Control.Monad.State
    import Control.Monad(join)

    import qualified Data.Map as M
    import qualified Data.Text.Lazy as T

    type Entry = (JSON,JSON)

    data Rule = Rule {
        target :: T.Text,
        prerequisites :: [T.Text],
        commands :: [T.Text]
    }deriving Show

    data MakeFileConfigs = MakeFileConfigs {
            typedefs :: M.Map T.Text T.Text,
            rules :: [Rule],
            phonies :: [Rule]
    }deriving Show

    runProgram :: JSON -> MakeFileConfigs
    runProgram js = execState (iterateJson js) emptyFileConfigs 

    mapJsonArray :: (MonadState MakeFileConfigs m,Monad m) => (JSON -> m a) -> JSON -> m ()
    mapJsonArray f js = case js of
        JSARRAY lst -> mapM_ f lst
        _ -> return ()    

    mapJsonObject :: (MonadState MakeFileConfigs m,Monad m) => (Entry -> m ()) -> JSON -> m ()
    mapJsonObject f js = case js of
        JSOBJECT lst -> mapM_ f lst
        _ -> return ()    

    resolve :: JSON -> M.Map T.Text T.Text -> T.Text
    resolve js table =
        case js of
            (JSVAR var) -> case M.lookup var table of
                                Nothing -> T.pack ""
                                Just a -> a
            (JSSTRING str) -> str
            (JSARRAY strs) -> T.intercalate (T.pack " ") $ map (`resolve` table) strs
            _ -> undefined

    addDef,addRule,addPhony :: (MonadState MakeFileConfigs m,Monad m) => Entry -> m ()
    addDef (JSVAR var,str) = () <$
        (modify $ \s ->
            s {typedefs = M.insert var (resolve str $ typedefs s) $ typedefs s})


    addRule (targ,JSOBJECT entries) = do
        symbol <- gets $ \s -> resolve targ (typedefs s)
        s <- get
        case M.lookup symbol (typedefs s) of
                Just _ -> error $ "target" <> T.unpack symbol <> "already exists"
                Nothing -> do
                        modify $ \s -> s {rules = (foldl f emptyRule entries) : rules s}
                        where f r entry = case entry of
                                    (JSSTRING a,deps) | a == T.pack "deps" -> 
                                            r {prerequisites = jsonArrayToList (typedefs s) deps}
                                    (JSSTRING a,cmds) | a == T.pack "commands" -> 
                                            r {commands = (commands r) ++ 
                                                        [resolve cmds $ typedefs s]

                                              }
                                    (JSSTRING a,_) -> error $ "invalid rule field" <> show a
                              emptyRule = Rule symbol [] []
                                        
    addPhony (targ,JSOBJECT entries) = do
           symbol <- gets $ \s -> resolve targ (typedefs s)
           s <- get
           case M.lookup symbol (typedefs s) of
                   Just _ -> error $ "phony" <> T.unpack symbol <> "already exists"
                   Nothing -> do
                           modify $ \s -> s {phonies = (foldl f emptyRule entries) : phonies s}
                           where f r entry = case entry of
                                       (JSSTRING a,deps) | a == T.pack "deps" -> 
                                               r {prerequisites = jsonArrayToList (typedefs s) deps}
                                       (JSSTRING a,cmds) | a == T.pack "commands" -> 
                                               r {commands = (commands r) ++ 
                                                           [resolve cmds $ typedefs s]

                                                 }
                                       (JSSTRING a,_) -> error $ "invalid rule field" <> show a
                                 emptyRule = Rule symbol [] []
                                               
                                        

    jsonArrayToList :: M.Map T.Text T.Text -> JSON -> [T.Text]
    jsonArrayToList table (JSARRAY lst) =
        map (flip resolve table) lst
    jsonArrayToList table (JSSTRING lst) =
        T.words lst
    jsonArrayToList table (JSVAR lst) =
        T.words $ resolve (JSVAR lst) table
    jsonArrayToList _ _ = undefined




    iterateJson :: (MonadState MakeFileConfigs m,Monad m) => JSON -> m ()
    iterateJson = mapJsonObject f
        where 
            f (a,b) = case a of
                            JSSTRING q | q == T.pack "rules" -> mapJsonObject addRule b
                            JSSTRING q | q == T.pack "declarations" -> mapJsonObject addDef b
                            JSSTRING q | q == T.pack "phonies" -> mapJsonObject addPhony b
                            _ -> undefined



    emptyFileConfigs = MakeFileConfigs (M.fromList []) [] []

    
