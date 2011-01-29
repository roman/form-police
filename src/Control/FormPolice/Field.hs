module Control.FormPolice.Field 
  ( Field
  , createField
  , getName
  ) where

  import           Data.Text (Text)
  
  data Field = 
    Field {
      fieldName :: Text
    }

  createField :: Text -> Field
  createField name = Field name

  getName :: Field -> Text
  getName = fieldName
