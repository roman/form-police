module Control.FormPolice.Field 
  ( Field
  , createField
  , getName
  , getValue
  , setValue
  ) where

  import           Data.Text (Text)
  import           Data.Aeson (Value, ToJSON (..))
  
  data Field = 
    Field {
      fieldName :: Text
    , fieldValue :: Maybe Value
    }

  createField :: Text -> Field
  createField name = Field name Nothing

  getName :: Field -> Text
  getName = fieldName

  getValue :: Field -> Maybe Value
  getValue = fieldValue

  setValue :: (ToJSON a) => a -> Field -> Field
  setValue value field = field { fieldValue = Just (toJSON value) } 
