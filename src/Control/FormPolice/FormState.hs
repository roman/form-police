module Control.FormPolice.FormState 
  ( FormState
  , createState
  , getParams 
  ) where
  
  import          Data.Aeson (Value(..), Object)  

  data FormState 
    = FormState {
      fsParams :: Value
    }

  createState :: Value -> FormState
  createState params = FormState params

  getParams :: FormState -> Object
  getParams formState = 
    case fsParams formState of
      (Object o) -> o
      _          -> error "peligro!"
