module PushMV.Test.File where
import Test.Framework(defaultMain,testGroup)
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit( (@=?) )
import PushMV.File
import Data.List

testGroup_PushMV_File = testGroup "PushMV.File" [
             testCase "Dot file don't include." dotfilenot_case 
            ,testCase "Oldest File Pull" oldestfilepull]
        where
            dotfilenot_case = (["File.hs"] @=? notdotfile ["File.hs",".",".."])
            oldestfilepull  = ([("hoge",1),("Fuga",2),("Poko",3)] @=? sortBy sortGT [("Fuga",2),("Poko",3),("hoge",1)])
