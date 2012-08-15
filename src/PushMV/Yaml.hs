module PushMV.Yaml (readYaml
                    ,configureMaybeNextDir
                    ,configure_notdir
                    ,configure_notfile
                    ,ConfigData,ConfigNextDir) where

import System.IO
import System.Directory
import Data.Yaml.Syck

data ConfigData = Config ConfigNextDir ConfigLimit NotDirectoryInclude NotFileInclude
type ConfigNextDir = Maybe String
type ConfigLimit   = Maybe String
type NotDirectoryInclude = Bool
type NotFileInclude = Bool

configureNextDir :: ConfigData -> ConfigNextDir
configureNextDir (Config x _ _ _)  = x

configureMaybeNextDir :: Maybe ConfigData -> ConfigNextDir
configureMaybeNextDir maybeconfig = case maybeconfig of
    Just conf -> configureNextDir conf
    Nothing   -> Nothing

configure_limit :: ConfigData -> ConfigLimit
configure_limit (Config _ x _ _) = x

configure_notdir :: ConfigData -> NotDirectoryInclude
configure_notdir (Config _ _ x _) = x

configure_notfile :: ConfigData -> NotFileInclude
configure_notfile (Config _ _ _ x) = x

configFileLoad :: String -> IO (Maybe YamlNode)
configFileLoad x = doesFileExist x >>= (\is_exist -> 
                   if is_exist 
                        then  (\b -> return $ Just b) =<< parseYamlFile x
                        else return Nothing)

configYaml :: String -> IO (Maybe [(YamlNode,YamlNode)])
configYaml x = configFileLoad x >>= (\is_yamlfile ->
                case is_yamlfile of
                    Just x ->  return (Just(unpackyaml x))
                    Nothing -> return Nothing)

unpackyaml :: YamlNode -> [(YamlNode,YamlNode)]
unpackyaml x = case n_elem x of 
                    EMap list -> list
                    otherwise -> []


wrapperConfig :: [(YamlNode,YamlNode)] -> ConfigData
wrapperConfig x = Config (getParam x "next") (getParam x "limit") (getParamBool x "notdir") (getParamBool x "notfile")

readYaml :: String -> IO (Maybe ConfigData)
readYaml x = configYaml x >>= (\yamldata ->
                case yamldata of
                    Just x -> return $ Just (wrapperConfig x)
                    Nothing -> return Nothing)

getParamBool :: [(YamlNode,YamlNode)] -> String -> Bool
getParamBool x y = case x of
                    [] -> False
                    otherwise -> case (getParam x y) of
                                    Just hasParam -> hasParam == "true"
                                    Nothing -> False

getParam :: [(YamlNode,YamlNode)] -> String -> Maybe String
getParam x y = case x of
                    []     -> Nothing
                    (z:zx) -> case (getKey z y) of
                                Just is_find  -> Just is_find
                                Nothing -> getParam zx y

getKey :: (YamlNode,YamlNode) -> String -> Maybe String 
getKey x y = case keyEStr of 
                EStr z
                    | (unpackBuf $ z) == y -> case keyValue of
                                                EStr findValue -> Just(unpackBuf findValue)
                                                otherwise -> Nothing
                    | otherwise -> Nothing
                otherwise -> Nothing
                where 
                    keyEStr = n_elem $ fst x
                    keyValue = n_elem $ snd x
