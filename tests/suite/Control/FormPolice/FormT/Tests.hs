module Control.FormPolice.FormT.Tests 
  (tests) where

  import           Test.Framework (Test)
  import           Test.Framework.Providers.HUnit (testCase)

  import qualified Data.Map as O

  import           Data.Text (Text)

  import           Test.HUnit (assertEqual)

  import           Control.FormPolice.FormT

  tests :: [Test]
  tests = [ testRunFormT ]


  testRunFormT :: Test
  testRunFormT = testCase "runFormT should run successfuly" $ do
    let value = ("" :: Text)
    (result, _) <- runFormT (return "") O.empty
    assertEqual "runFormT doesn't execute correctly" value result


