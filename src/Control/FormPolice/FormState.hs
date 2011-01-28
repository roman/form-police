module Control.FormPolice.FormState where
  
  import          Data.Aeson (Object)  

  data FormState 
    = FormState {
      fsParam :: Object
    }

  createState :: Object -> FormState
  createState params = FormState params
