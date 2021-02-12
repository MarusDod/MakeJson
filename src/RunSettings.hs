module RunSettings where

    import System.Process(system)
    import GHC.IO.Exception(ExitCode(..))
    import System.Directory(doesPathExist)
    import System.Posix.Files(getFileStatus,modificationTime)

    import Control.Monad(liftM2,forM,forM_,when,unless)
    import Control.Monad.Trans.Except
    import Control.Monad.Trans(lift)
    import Data.Function(on)
    import Data.List(find)
    import qualified Data.Text.Lazy as T

    import LoadSettings(MakeFileConfigs(..),Rule(..),emptyFileConfigs)

    type ExceptIO a = ExceptT String IO a
    

    runRule,runPhony :: MakeFileConfigs -> Rule -> ExceptIO ()
    runRule config rule =
        let cur_target = T.unpack $ target rule
        in do
        withExceptT (\e -> "failed target '" <> cur_target <> "':\n\t" <> e)
            (forM_ (T.unpack <$> prerequisites rule) 
                    (\r -> lift (isUpToDate r cur_target) >>= \cond ->
                        unless cond $
                            case searchTarget r config of
                                Nothing -> do
                                    condExists <- lift $ doesPathExist r
                                    if condExists
                                        then return ()
                                        else throwE $ "no target '" <> r <> "'found"
                                Just newrule -> runRule config newrule))
        forM_ (T.unpack <$> commands rule) $
            \cmd -> do
                exitcode <- lift $ system cmd
                case exitcode of
                    ExitSuccess -> return ()
                    ExitFailure ret -> throwE $
                        "invalid shell command '" <> cmd <> "' exited with code " <> show ret
                        
    runPhony = runRule


    searchTarget :: String -> MakeFileConfigs -> Maybe Rule
    searchTarget str config =
        flip find (rules config) $
            \r -> target r == T.pack str
            
    searchPhony str config =
            flip find (phonies config) $
                \r -> target r == T.pack str


    isUpToDate :: String -> String -> IO Bool
    isUpToDate str cur_target = do
            b <- (&&) <$> doesPathExist str <*> doesPathExist cur_target
            if b 
            then      
                liftM2 ((<) `on` modificationTime)
                    (getFileStatus str) 
                    (getFileStatus cur_target)
            else return False


    example :: MakeFileConfigs
    example = emptyFileConfigs {rules = [Rule (T.pack "a.out") (T.pack <$> ["main.c"]) (T.pack <$> ["gcc main.c"])]}
