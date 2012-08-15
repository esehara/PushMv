module PushMV.Test where

import Test.Framework(defaultMain,testGroup)
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit( (@=?) )
import Data.List

import PushMV
import PushMV.Util
import PushMV.Yaml
import PushMV.Test.File

main :: IO ()
main =  defaultMain tests

tests = [testGroup_testrunning
        ,testGroup_testDublicatePath
        ,testGroup_PushMV_File
        ,testGroup_testTargetDirectory]

testGroup_testrunning = testGroup "Test is " [
    testCase "Run." (1 @=? 1)
    ]

testGroup_testDublicatePath = testGroup "Dublicate Path is" 
    [
         testCase "None Have, Return Maybe[String] ." (maybePastProcessDirectory "Hoge" [] @=? Just ["Hoge"])
        ,testCase "Have, Return Nothing." (maybePastProcessDirectory "hoge" ["hoge"] @=? Nothing)
    ]

testGroup_testTargetDirectory = testGroup "If TargetDirectory is"
    [testCase "Nothing,return Nothing"
     (maybeTargetDirectory Nothing @=? Nothing)
    ,testCase "Have, return New Path." (
     (maybeTargetDirectory $ 
      maybePastProcessDirectory "hoge" ["fuga","poko"] 
     ) @=? Just "hoge")
    ]

