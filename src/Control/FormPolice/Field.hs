module Control.FormPolice.Field 
  ( Field
  , createField
  , getName
  , getValue
  , setValue
  , getErrors
  , appendError
  ) where

  import           Data.Text (Text)
  import           Data.Aeson (Value, ToJSON (..))
  
  data Field = 
    Field {
      fieldName   :: Text
    , fieldValue  :: Maybe Value
    , fieldErrors :: [Text]
    }

  createField :: Text -> Field
  createField name = Field name Nothing []

  getName :: Field -> Text
  getName = fieldName

  getValue :: Field -> Maybe Value
  getValue = fieldValue

  setValue :: (ToJSON a) => a -> Field -> Field
  setValue value field = field { fieldValue = Just (toJSON value) } 

  getErrors :: Field -> [Text]
  getErrors = fieldErrors

  appendError :: Text -> Field -> Field
  appendError errMsg field = field { fieldErrors = (errMsg:) $ fieldErrors field }

