module Control.FormPolice.Validations.Tests 
  (tests) where

  import            Test.Framework (Test)
  import            Test.Framework.Providers.HUnit (testCase)
  import            Test.HUnit (assertBool)

  import           Data.Text (Text)
  import qualified Data.Text as T
  import           Data.Aeson (object, (.=))
 
  import qualified Control.FormPolice.FieldMap  as FM
  import qualified Control.FormPolice.FormState as FS
  import           Control.FormPolice.Validations
  import           Control.FormPolice.FormT
 
  tests :: [Test]
  tests = [ testValidate
          ]
 
  testValidate :: Test
  testValidate = testCase "validate creates validators for fields" $ do
     let validation = validate (not . T.null) "can't be blank" 
     (_, formState) <- runFormT (text "name" `validates` [validation]) (object ["name" .= ("" :: Text)])
     let errors = FM.mapErrors (,) . FS.getFieldMap $ formState :: [(Text, [Text])]
     assertBool "validate is not working correctly" (not $ null errors)
     assertBool "validate doesn't add correct value to the field" $ ("name", ["can't be blank"]) `elem` errors
     
  
