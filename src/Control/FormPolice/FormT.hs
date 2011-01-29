module Control.FormPolice.FormT 
  ( FormState
  , FormT
  , runFormT

  , getParam

  , createField
  , setFieldValue

  ) where

  import           Data.Text (Text)
  import           Data.Aeson (ToJSON(..), FromJSON, Object, Value, (.:))

  import           Control.Monad (liftM)
  import           Control.Monad.State (StateT, runStateT, get, put)

  import           Control.FormPolice.FormState (FormState)
  import qualified Control.FormPolice.FormState as FS

  import           Control.FormPolice.Field (Field)
  import qualified Control.FormPolice.Field as F

  newtype FormT m a = FormT (StateT FormState m a) deriving (Monad)

  runFormT :: (Monad m) => FormT m a -> Value -> m (a, FormState)
  runFormT (FormT m) value = runStateT m (FS.createState value)

  getParams :: (Monad m) => FormT m Object
  getParams = FormT (FS.getParams `liftM` get)

  getParam :: (FromJSON a, Monad m) => Text -> FormT m (Maybe a)
  getParam key = (.: key) `liftM` getParams

  --
  --
  alterFormState :: (Monad m) => (FormState -> FormState) -> FormT m ()
  alterFormState fn = FormT (get >>= put . fn)

  alterFormField :: (Monad m) => (Field -> Field) -> FormT m ()
  alterFormField fn = alterFormState helper
    where
      helper formState = FS.setCurrentField (fn `liftM` (FS.getCurrentField formState)) formState
    
  createField :: (Monad m) => Text -> FormT m ()
  createField name = alterFormState (FS.setCurrentField (Just $ F.createField name))

  setFieldValue :: (Monad m, ToJSON a) => a -> FormT m ()
  setFieldValue value = alterFormField (F.setValue value)
  
