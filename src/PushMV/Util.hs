module PushMV.Util where

import PushMV.Yaml
import PushMV.File
import System.Directory

oldestFile :: IO [FilePath] -> IO (Maybe FilePath)
oldestFile iofilepath = do
            filepath <- iofilepath
            sortfilepath <- ioSortFileTimeList $ ioFileTimeList $ notdotfile filepath 
            if sortfilepath == [] 
                then return Nothing
                else return $ Just $ fst $ head sortfilepath



isExistNextDir :: Maybe String -> IO (Maybe String)
isExistNextDir targetPath = case targetPath of
    Nothing   -> return Nothing
    Just path -> do
                    isExist <- doesDirectoryExist path
                    if isExist
                        then return $ Just path
                        else return Nothing

currentDirectoryFileList :: IO [FilePath]
currentDirectoryFileList = do
    path <- getCurrentDirectory
    contentslist <- getDirectoryContents path
    return $ notdotfile contentslist

currentFileListforUse :: Maybe ConfigData -> IO [FilePath] 
currentFileListforUse configYaml = case configYaml of
        Nothing       -> return []
        Just configuredata -> do
           filelist <- currentDirectoryFileList
           ioBoolToFilePath $ 
                filterIOBool (configure_notfile configuredata) True $
                filterIOBool (configure_notdir configuredata) False $ 
                ioFileDirectoryorNotList filelist

unitProcessPushMoving :: Maybe String -> IO (Maybe String)
unitProcessPushMoving maybetargetpathConfigYaml = case maybetargetpathConfigYaml of 
    Nothing -> return Nothing
    Just targetpathConfigYaml -> do
    -- Do Block Start
    temporaryConfigYaml <- readYaml targetpathConfigYaml
    -- 更新の一番古いファイルを取得する
    targetFile <- oldestFile $ currentFileListforUse temporaryConfigYaml
    -- ファイルをNextDirに移動する
    -- 次のPathをReturnする 
    return $ configureMaybeNextDir temporaryConfigYaml
