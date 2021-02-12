{-# LANGUAGE OverloadedStrings #-}

module Main where

    import System.Environment(getArgs)
    import Control.Monad(when,forM_,void)
    import Control.Monad.State(runState)
    import Control.Monad.Trans.Except(runExceptT,catchE)
    import Control.Monad.Trans.Class(lift)
    import Data.List(find)
    import qualified Data.Map as M

    import qualified Data.Text.Lazy.IO as T
    import qualified Data.Text.Lazy as T
    import System.IO(hPutStrLn,stderr)

    import ParseSettingsJson
    import LoadSettings
    import RunSettings(runRule)
    
    configFile :: String
    configFile = "config.json"

    main = do
        js <- jsonParseFile configFile
        args <- getArgs
        case js of
             Left e -> putStrLn e
             Right js ->
                let config = runProgram js
                in 
                forM_ (if (null args) then (maybe [] (return . T.unpack)  $ M.lookup "all" $ typedefs config) else args) $ \str ->
                    case find (\r -> T.pack str == target r) (phonies config <> rules config) of
                        Nothing -> putStrLn $ "No target named " <> str
                        Just rule -> void . runExceptT $
                                catchE 
                                    (runRule config rule)
                                    (lift . hPutStrLn stderr)
