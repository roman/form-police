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
  , getPossibleValues
  , setPossibleValues
  , getChildrenFieldMap
  , setChildrenFieldMap
  ) where

  import           Data.Text (Text)
  import           Data.Aeson (Value, ToJSON (..))

  import           Data.Map (Map)
  import qualified Data.Map as M


  data FieldType 
    = TextField
    | TextareaField
    | PasswordField
    | CheckboxField
    | SelectField
    | RadioField
    deriving (Show, Eq)
  
  data Field = 
    Field {
      fieldName   :: Text
    , fieldValue  :: Maybe Value
    , fieldErrors :: [Text]
    , fieldType   :: FieldType
    , fieldPossibleValues :: [(Text, Text)]
    , fieldChildren :: Map Text Field
    }
    deriving (Show)


  createField :: Text -> Field
  createField name = Field name Nothing [] TextField [] M.empty

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

  getPossibleValues :: Field -> [(Text, Text)]
  getPossibleValues = fieldPossibleValues

  setPossibleValues :: [(Text, Text)] -> Field -> Field
  setPossibleValues values field = field { fieldPossibleValues = values }

  getChildrenFieldMap :: Field -> Map Text Field
  getChildrenFieldMap = fieldChildren

  setChildrenFieldMap :: Map Text Field -> Field -> Field
  setChildrenFieldMap fieldMap field = field { fieldChildren = fieldMap }

