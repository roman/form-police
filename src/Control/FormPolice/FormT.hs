module Control.FormPolice.FormT 
  ( FormState
  , FormT
  , runFormT

  , getParam

  , createField
  , getFieldValue
  , setFieldValue
  , getFieldErrors
  , appendFieldError

  ) where

  import           Data.Text (Text)
  import           Data.Aeson (ToJSON(..), FromJSON(..), Object, Value, (.:))
  import           Data.Monoid (Monoid, mempty)

  import           Control.Monad (liftM, (>=>))
  import           Control.Monad.State (StateT, runStateT, get, put)
  import           Control.Arrow ((>>>))

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
  getFormState :: (Monad m) => FormT m FormState
  getFormState = FormT get

  getCurrentField :: (Monad m) => FormT m (Maybe Field)
  getCurrentField = FS.getCurrentField `liftM` getFormState

  alterFormState :: (Monad m) => (FormState -> FormState) -> FormT m ()
  alterFormState fn = FormT (get >>= put . fn)

  alterCurrentField :: (Monad m) => (Field -> Field) -> FormT m ()
  alterCurrentField fn = alterFormState helper
    where
      helper formState = FS.setCurrentField (fn `liftM` (FS.getCurrentField formState)) formState
    
  createField :: (Monad m) => Text -> FormT m ()
  createField name = alterFormState (FS.setCurrentField (Just $ F.createField name))

  getFieldValue :: (Monad m, Monoid a, FromJSON a) => FormT m a
  getFieldValue = getFormState >>= ((FS.getCurrentField >=> F.getValue >=> fromJSON) >>> maybe mempty id >>> return)

  setFieldValue :: (Monad m, ToJSON a) => a -> FormT m ()
  setFieldValue value = alterCurrentField (F.setValue value)

  getFieldErrors :: (Monad m) => FormT m [Text]
  getFieldErrors = getCurrentField >>= ((F.getErrors `liftM`) >>> maybe mempty id >>> return)

  appendFieldError :: (Monad m) => Text -> FormT m ()
  appendFieldError errMsg = alterCurrentField (F.appendError errMsg)

