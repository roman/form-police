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
  , commitField
  , setFieldType
  , setFieldPossibleValues
  , pushToChild
  , createFormField 

  ) where

  import           Data.Text (Text)
  import           Data.Aeson (ToJSON(..), FromJSON(..), Object, Value, (.:))
  import           Data.Monoid (Monoid, mempty)

  import           Control.Monad (liftM, (>=>))
  import           Control.Monad.State (StateT, runStateT, get, put)
  import           Control.Arrow ((>>>))

  import           Control.FormPolice.FormState (FormState)
  import qualified Control.FormPolice.FormState as FS
  import           Control.FormPolice.Field (Field, FieldType(..))
  import qualified Control.FormPolice.Field as F
  import qualified Control.FormPolice.FieldMap as FM
  import           Control.FormPolice.MonadForm

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

  setFormState :: (Monad m) => FormState -> FormT m ()
  setFormState = FormT . put

  getCurrentField :: (Monad m) => FormT m (Maybe Field)
  getCurrentField = FS.getCurrentField `liftM` getFormState

  alterFormState :: (Monad m) => (FormState -> FormState) -> FormT m ()
  alterFormState fn = FormT (get >>= put . fn)

  localFormState :: (Monad m) => (FormState -> FormState) -> FormT m a -> FormT m a
  localFormState fn action = do
    formState <- getFormState
    setFormState (fn formState)
    a <- action 
    setFormState formState
    return a

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

  commitField :: (Monad m) => FormT m ()
  commitField = alterFormState helper
    where
      helper formState = 
        let field    = FS.getCurrentField formState 
            fieldMap = FS.getFieldMap formState
        in maybe formState (flip FS.setFieldMap formState . flip FM.insert fieldMap) field

  setFieldType :: (Monad m) => FieldType -> FormT m ()
  setFieldType fieldType = alterCurrentField (F.setFieldType fieldType)

  setFieldPossibleValues :: (Monad m) => [(Text, Text)] -> FormT m ()
  setFieldPossibleValues values = alterCurrentField (F.setPossibleValues values)

  pushToChild :: (Monad m) => Text -> FormT m a -> FormT m a
  pushToChild name action = do
    result <- getParam name
    case result of
      Just object -> localFormState (FS.setParams object) action
      Nothing     -> action

  transaction :: (Monad m) => FormT m a -> FormT m a
  transaction action = action >>= \a -> commitField >> return a
  
  createFormField :: (ToJSON a, FromJSON a, Monoid a, Monad m) => FieldType -> Text -> FormT m a
  createFormField fieldType fieldName = transaction $ do
    createField fieldName 
    setFieldType fieldType
    result <- getParam fieldName
    case result of
      Nothing    -> return mempty
      Just value -> do 
        setFieldValue value
        return value


  instance (Monad m) => MonadForm (FormT m) where
    text     = createFormField TextField
    password = createFormField PasswordField
    textarea = createFormField TextareaField
    checkbox = createFormField CheckboxField


