module Control.FormPolice.Field.Types 
  ( Field (..)
  , FieldType (..)
  , FieldMap 
  , M.empty
  ) where

  import           Data.Text (Text)
  import           Data.Aeson (Value)

  import           Data.Map (Map)
  import qualified Data.Map as M


  data Field = 
    Field {
      fieldName   :: Text
    , fieldValue  :: Maybe Value
    , fieldErrors :: [Text]
    , fieldType   :: FieldType
    , fieldPossibleValues :: [(Text, Text)]
    , fieldChildren :: FieldMap
    }
    deriving (Show)

  data FieldType
    = TextField
    | TextareaField
    | PasswordField
    | CheckboxField
    | SelectField
    | RadioField
    deriving (Show, Eq)

  type FieldMap = Map Text Field



