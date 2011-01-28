module Main where

  import           Test.Framework (defaultMain, testGroup)

  import qualified Control.FormPolice.FormT.Tests
  
  main :: IO ()
  main = defaultMain tests
    where
      tests = [ testGroup "Control.FormPolice.FormT.Tests.tests"
                           Control.FormPolice.FormT.Tests.tests
              ]
