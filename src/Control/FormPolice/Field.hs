module Control.FormPolice.Field 
  ( Field
  , FieldType (..)
  , createField
  , getName
  , getValue
  , setValue
  , getErrors
  , appendError
  , getFieldType
  , setFieldType
  ) where

  import           Data.Text (Text)
  import           Data.Aeson (Value, ToJSON (..))

  data FieldType 
    = TextField
    | TextareaField
    | PasswordField
    | CheckboxField
    deriving (Show, Eq)
  
  data Field = 
    Field {
      fieldName   :: Text
    , fieldValue  :: Maybe Value
    , fieldErrors :: [Text]
    , fieldType   :: FieldType
    }

  createField :: Text -> Field
  createField name = Field name Nothing [] TextField

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

  getFieldType :: Field -> FieldType
  getFieldType = fieldType

  setFieldType :: FieldType -> Field -> Field
  setFieldType fType field = field { fieldType = fType }

