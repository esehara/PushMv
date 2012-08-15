module PushMV where

import System.Environment
import System.Directory
import PushMV.Yaml
import PushMV.File
{--
main :: IO ()
main = entryPoint >>=
       (\path -> readYamlData path >>= putStrLn)
--}

{--
entryPoint :: IO [Char]
entryPoint = getArgs >>= (\x ->
             getCurrentDirectory >>= 
             (\path -> case x of
                [] -> return path
                otherwise -> is_first_slash (head x) >>= return))

--} 

initLoopProcess :: String -> IO ()
initLoopProcess argpath = do
    mainLoopProcess (ioSetDirectory argpath) []

mainLoopProcess ioprocesspath processlist = do
    processpath <- ioprocesspath
    nowpath     <- setWorkingPath processpath
    putStrLn ("---- Current Directory ----")
    putStrLn (show nowpath)
    yamldata <- maybeReadYamlData nowpath
    putStrLn ("---- Next Directory ----")
    nextdir <- maybe_first_slash $ configureMaybeNextDir yamldata    
    putStrLn $ show nextdir
        where
            setWorkingPath processpath = case (
                  maybeTargetDirectory 
                $ maybePastProcessDirectory processpath processlist) of
                    Just path  -> (ioSetDirectory path) >>= (\result -> return $ Just result)
                    Nothing    -> return Nothing



ioSetDirectory :: String -> IO FilePath
ioSetDirectory "" = getCurrentDirectory
ioSetDirectory x  = setCurrentDirectory x >> getCurrentDirectory

maybePastProcessDirectory :: String -> [FilePath] -> Maybe [FilePath]
maybePastProcessDirectory currentpath pastpathlist
    | (currentpath `elem` pastpathlist) == True = Nothing
    | otherwise                                 = Just $ currentpath : pastpathlist

maybeTargetDirectory :: Maybe [FilePath] -> Maybe String
maybeTargetDirectory getcurrentpath = 
    case getcurrentpath of 
        Just x -> Just $ head $ x
        Nothing -> Nothing


getCurrentFileList :: IO [FilePath] -> IO (Maybe [FilePath])
getCurrentFileList iofilepathlist = do
                    filepathlist <- iofilepathlist
                    if (length $ notdotfile $ filepathlist) == 0
                        then return Nothing
                        else return $ Just $ notdotfile $ filepathlist

maybeReadYamlData :: Maybe String -> IO (Maybe ConfigData)
maybeReadYamlData maybepath = case maybepath of 
    Just path -> readYamlData path
    Nothing   -> return Nothing

readYamlData :: String -> IO (Maybe ConfigData)
readYamlData path = readYaml ((is_last_slash path) ++ ".pushmv")
                    {--
                    >>= (\x -> case x of
                    Just configdata -> (
                        maybe_first_slash $
                        configureMaybeNextDir $ Just configdata) >>= 
                        (\returnvalue -> return returnvalue)
                    Nothing         -> return Nothing
                    )
                    --}

--
-- Path Varitation 
--

maybe_first_slash :: ConfigNextDir -> IO ConfigNextDir
maybe_first_slash maybepath = case maybepath of 
   Just have_path -> _first_slash have_path >>=
                     (\getpath -> return $ Just getpath)
   Nothing        -> return Nothing

_first_slash :: String -> IO String
_first_slash x
    | (head x) == '/' = return x
    | otherwise       = getCurrentDirectory >>= (\currentpath -> return $ ((currentpath ++ "/") ++ x))

is_last_slash :: String -> String
is_last_slash x 
    | (last x) == '/' = x
    | otherwise       = x ++ "/"
