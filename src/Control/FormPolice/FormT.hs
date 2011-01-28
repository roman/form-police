module Control.FormPolice.FormT 
  ( 
    FormT
  , runFormT
  , FormState
  ) where

  import           Data.Aeson (Object)

  import           Control.Monad.State (StateT, runStateT)

  import           Control.FormPolice.FormState (FormState)
  import qualified Control.FormPolice.FormState as FS

  newtype FormT m a = FormT (StateT FormState m a) deriving (Monad)

  runFormT :: (Monad m) => FormT m a -> Object -> m (a, FormState)
  runFormT (FormT m) value = runStateT m (FS.createState value)

  
