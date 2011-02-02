module Main where

  import           Test.Framework (defaultMain, testGroup)

  import qualified Control.FormPolice.FormT.Tests
  import qualified Control.FormPolice.Validations.Tests
  
  main :: IO ()
  main = defaultMain tests
    where
      tests = [ testGroup "Control.FormPolice.FormT.Tests.tests"
                           Control.FormPolice.FormT.Tests.tests
              , testGroup "Control.FormPolice.Validations.Tests.tests"
                           Control.FormPolice.Validations.Tests.tests
              ]
