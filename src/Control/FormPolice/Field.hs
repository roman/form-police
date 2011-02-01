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

  import           Control.FormPolice.Field.Types (Field(..), FieldMap, empty, FieldType(..))
  
  createField :: Text -> Field
  createField name = Field name Nothing [] TextField [] empty

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

  getChildrenFieldMap :: Field -> FieldMap
  getChildrenFieldMap = fieldChildren

  setChildrenFieldMap :: FieldMap -> Field -> Field
  setChildrenFieldMap fieldMap field = field { fieldChildren = fieldMap }

