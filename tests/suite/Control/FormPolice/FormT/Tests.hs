module Control.FormPolice.FormT.Tests 
  (tests) where

  import           Test.Framework (Test)
  import           Test.Framework.Providers.HUnit (testCase)
  import           Test.HUnit (assertEqual, assertBool)

  import           Data.Text (Text)
  import           Data.Aeson (FromJSON(..), Value, object, (.=))
  import           Data.Maybe (fromJust, isJust, isNothing)

  import           Control.Monad ((>=>))

  import qualified Control.FormPolice.FormState as FS
  import qualified Control.FormPolice.Field as F
  import           Control.FormPolice.FormT

  tests :: [Test]
  tests = [ testRunFormT 
          , testGetParamReturnsValue 
          , testGetParamReturnsNothing 
          , testCreateField
          , testSetFieldValue 
          ]
  
  emptyObject :: Value
  emptyObject = object []

  testRunFormT :: Test
  testRunFormT = testCase "runFormT executes successfuly" $ do
    let value = ("" :: Text)
    (result, _) <- runFormT (return "") emptyObject
    assertEqual "runFormT doesn't execute correctly" value result

  testGetParamReturnsValue :: Test
  testGetParamReturnsValue = testCase "getParam returns 'Just value' from form JSON object" $ do
    let value = "john" :: Text
    (result, _) <- runFormT (getParam "name") (object ["name" .= value])
    assertEqual "getParam is not returning correct value" value (fromJust result)

  testGetParamReturnsNothing :: Test
  testGetParamReturnsNothing = testCase "getParam returns 'Nothing' when key not on form JSON object" $ do
    (result, _) <- runFormT (getParam "name") emptyObject
    assertBool "getParam is not returning 'Nothing'" (isNothing (result :: Maybe Text))

  testCreateField :: Test
  testCreateField = testCase "createField registers a current field in the FormState" $ do
    (_, formState) <- runFormT (createField "name") emptyObject
    let field = FS.getCurrentField formState
    assertBool  "createField is not creating a current field" (isJust field)
    assertEqual "createField has an invalide name" "name" (F.getName $ fromJust field)

  testSetFieldValue :: Test
  testSetFieldValue = testCase "setFieldValue modifies value of current field in the FormState" $ do
    let fieldValue = ("john" :: Text)
    (_, formState) <- runFormT (createField "name" >> setFieldValue fieldValue) emptyObject
    let resultValue = maybe "" id $ (FS.getCurrentField >=> F.getValue >=> fromJSON) formState
    assertEqual "setFieldValue is not setting correct value in FormState's current field" fieldValue resultValue

