module Control.FormPolice.FormState 
  ( FormState
  , createState
  , getParams 
  , getCurrentField
  , setCurrentField
  , getFieldMap
  , setFieldMap
  ) where
  
  import           Data.Aeson (Value(..), Object)  

  import           Control.FormPolice.Field (Field)
  import           Control.FormPolice.FieldMap (FieldMap)
  import qualified Control.FormPolice.FieldMap as FM

  data FormState 
    = FormState {
      fsParams :: Value
    , fsCurrentField :: Maybe Field
    , fsFieldMap :: FieldMap
    }

  createState :: Value -> FormState
  createState params = FormState params Nothing FM.empty

  getParams :: FormState -> Object
  getParams formState = 
    case fsParams formState of
      (Object o) -> o
      _          -> error "peligro!"

  getCurrentField :: FormState -> Maybe (Field)
  getCurrentField = fsCurrentField

  setCurrentField :: Maybe Field -> FormState -> FormState
  setCurrentField field formState = formState { fsCurrentField = field }

  getFieldMap :: FormState -> FieldMap
  getFieldMap = fsFieldMap

  setFieldMap :: FieldMap -> FormState -> FormState
  setFieldMap fieldMap formState = formState { fsFieldMap = fieldMap }

