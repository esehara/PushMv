module PushMV.File (ioSortFileTimeList
                    ,ioFileTimeList
                    ,ioFileDirectoryorNotList
                    ,ioBoolToFilePath
                    ,filterIOBool
                    ,notdotfile
                    ,sortGT) where

import System.Directory
import System.Time
import Data.List


notdotfile :: [FilePath] -> [FilePath]
notdotfile filelist = filter (\y -> ".." /= y) $ filter (\x -> "." /= x) filelist

sortGT :: (String,Integer) -> (String,Integer) -> Ordering
sortGT (a1,b1) (a2,b2)
    | b1 > b2 = GT
    | b1 < b2 = LT
    | b1 == b2 = compare b1 b2

ioSortFileTimeList :: IO [(String,Integer)] -> IO [(String,Integer)]
ioSortFileTimeList iolist = do
                raw_list <- iolist
                if raw_list == []
                then return []
                else return (sortBy sortGT raw_list)

ioFileTimeList :: [FilePath] -> IO [(String,Integer)]
ioFileTimeList [] = return []
ioFileTimeList filepathlist = do 
    foldl (+>>=+) (return []) filepathlist

ioFileDirectoryorNotList :: [FilePath] -> IO [(FilePath,Bool)]
ioFileDirectoryorNotList [] = return []
ioFileDirectoryorNotList filepathlist = do
    foldl (+/+) (return []) filepathlist

filterIOBool :: Bool -> Bool -> IO [(FilePath,Bool)] -> IO [(FilePath,Bool)]
filterIOBool use_filter filtBool ioList = case use_filter of
        False  -> ioList
        True -> do
                rawList <- ioList
                return $ filter (\gettuple -> (snd gettuple) == filtBool) rawList

ioBoolToFilePath :: IO [(FilePath,Bool)] -> IO [FilePath]
ioBoolToFilePath ioBoolList = do
                rawBoolList <- ioBoolList
                return $ map (\gettuple -> (fst gettuple)) rawBoolList

filetimeTuple :: String -> IO (String,Integer)
filetimeTuple x = getModificationTime x >>= (\(TOD sec _) -> return (x,sec))

wrapIoFileTimeStampList :: IO (String,Integer) -> IO [(String,Integer)]
wrapIoFileTimeStampList iox = do
                                raw_x <- iox
                                return [raw_x]


fileDirectoryorNotTuple :: String -> IO (String,Bool)
fileDirectoryorNotTuple filepath = do
    ioReturnBool <- doesDirectoryExist filepath
    return (filepath,ioReturnBool)

wrapioFileDirectoryorNotDirectory :: IO (String,Bool) -> IO [(String,Bool)]
wrapioFileDirectoryorNotDirectory iox = do
    raw_x <- iox
    return [raw_x]

(+/+) :: IO [(String,Bool)] -> FilePath -> IO[(String,Bool)]
(+/+) iolist elementpath = do
    tuplelist <- iolist
    gettuple <- wrapioFileDirectoryorNotDirectory $ fileDirectoryorNotTuple elementpath
    return (tuplelist ++ gettuple)

(+>>=+) :: IO [(String,Integer)] -> FilePath -> IO[(String,Integer)]
(+>>=+) iolist elementpath = do
                             tuplelist <- iolist
                             gettuple <- wrapIoFileTimeStampList $ filetimeTuple elementpath
                             return (tuplelist ++ gettuple)
