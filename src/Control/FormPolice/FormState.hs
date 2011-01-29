module Control.FormPolice.FormState 
  ( FormState
  , createState
  , getParams 
  , getCurrentField
  , setCurrentField
  ) where
  
  import          Data.Aeson (Value(..), Object)  

  import          Control.FormPolice.Field (Field)

  data FormState 
    = FormState {
      fsParams :: Value
    , fsCurrentField :: Maybe Field
    }

  createState :: Value -> FormState
  createState params = FormState params Nothing

  getParams :: FormState -> Object
  getParams formState = 
    case fsParams formState of
      (Object o) -> o
      _          -> error "peligro!"

  getCurrentField :: FormState -> Maybe (Field)
  getCurrentField = fsCurrentField

  setCurrentField :: Maybe Field -> FormState -> FormState
  setCurrentField field formState = formState { fsCurrentField = field }

