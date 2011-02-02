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

  , MonadForm (..)

  ) where

  import           Data.Text (Text)
  import           Data.Aeson (ToJSON(..), FromJSON(..), Object, Value(Object), (.:))
  import           Data.Monoid (Monoid, mempty)
  import           Data.Maybe (fromMaybe, fromJust)
  import qualified Data.Map as M
  import           Data.Functor

  import           Control.Monad (ap, liftM, (>=>))
  import           Control.Monad.State (StateT, runStateT, get, put)
  import           Control.Arrow ((>>>))
  import           Control.Applicative

  import           Control.FormPolice.FormState (FormState)
  import qualified Control.FormPolice.FormState as FS
  import           Control.FormPolice.Field (Field, FieldType(..))
  import qualified Control.FormPolice.Field as F
  import           Control.FormPolice.FieldMap (FieldMap)
  import qualified Control.FormPolice.FieldMap as FM
  import           Control.FormPolice.MonadForm

  newtype FormT m a = FormT (StateT FormState m a) deriving (Monad)

  runFormT :: (Monad m) => FormT m a -> Value -> m (a, FormState)
  runFormT (FormT m) value = runStateT m (FS.createState value)

  -- Accessors for the Monad
  -- 

  getFormState :: (Monad m) => FormT m FormState
  getFormState = FormT get

  alterFormState :: (Monad m) => (FormState -> FormState) -> FormT m ()
  alterFormState fn = FormT (get >>= put . fn)

  getCurrentField :: (Monad m) => FormT m (Maybe Field)
  getCurrentField = FS.getCurrentField `liftM` getFormState

  alterCurrentField :: (Monad m) => (Field -> Field) -> FormT m ()
  alterCurrentField fn = alterFormState helper
    where
      helper formState = FS.setCurrentField (fn `liftM` FS.getCurrentField formState) formState

  getParams :: (Monad m) => FormT m Object
  getParams = FS.getParams `liftM` getFormState

  setParams :: (Monad m) => Object -> FormT m ()
  setParams object = alterFormState (FS.setParams object)

  getFieldMap :: (Monad m) => FormT m FieldMap
  getFieldMap = FS.getFieldMap `liftM` getFormState

  setFieldMap :: (Monad m) => FieldMap -> FormT m ()
  setFieldMap fieldMap = alterFormState (FS.setFieldMap fieldMap)

  -- 
  -- 
  
  getParam :: (FromJSON a, Monad m) => Text -> FormT m (Maybe a)
  getParam key = (.: key) `liftM` getParams
    
  createField :: (Monad m) => Text -> FormT m ()
  createField name = alterFormState (FS.setCurrentField (Just $ F.createField name))

  getFieldValue :: (Monad m, Monoid a, FromJSON a) => FormT m a
  getFieldValue = getFormState >>= ((FS.getCurrentField >=> F.getValue >=> fromJSON) >>> fromMaybe mempty >>> return)

  setFieldValue :: (Monad m, ToJSON a) => a -> FormT m ()
  setFieldValue value = alterCurrentField (F.setValue value)

  getFieldErrors :: (Monad m) => FormT m [Text]
  getFieldErrors = getCurrentField >>= ((F.getErrors `liftM`) >>> fromMaybe mempty >>> return)

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

  transaction :: (Monad m) => FormT m a -> FormT m a
  transaction action = action >>= \a -> commitField >> return a

  createFormField :: (ToJSON a, FromJSON a, Monoid a, Monad m) => FieldType -> [(Text, Text)] -> Text -> FormT m a
  createFormField fieldType fieldPossibleValues fieldName = transaction $ do
    createField fieldName 
    setFieldType fieldType
    setFieldPossibleValues fieldPossibleValues
    result <- getParam fieldName
    case result of
      Nothing    -> return mempty
      Just value -> do 
        setFieldValue value
        return value

  pushToChild :: (Monoid a, Monad m) => Text -> FormT m a -> FormT m a
  pushToChild name action = transaction $ do
    value <- getParam name
    -- get a value for all different cases
    let object = case value of
                 Just (Object obj) -> obj
                 Just  _           -> M.empty
                 Nothing           -> M.empty
    -- register the field name on the FieldState
    createField name
    -- backup the current fieldMap and params
    field    <- fromJust `liftM` getCurrentField
    fieldMap <- getFieldMap
    params   <- getParams
    -- alter the state
    setFieldMap FM.empty
    setParams object
    -- execute action
    result <- action
    -- get important info from lower state
    childrenFieldMap <- getFieldMap
    -- restore state
    setFieldMap fieldMap
    setParams params
    let field' = F.setChildrenFieldMap childrenFieldMap field
    alterCurrentField (const field')
    -- return result from action
    return result

  -- FormT instances implementations
  --

  instance (Monad m) => MonadForm (FormT m) where
    appendError = transaction . appendFieldError
    getValue = getFieldValue

    text     = createFormField TextField []
    password = createFormField PasswordField []
    textarea = createFormField TextareaField []
    checkbox = createFormField CheckboxField []
    select   = flip (createFormField SelectField)
    radio    = flip (createFormField RadioField)
    child    = pushToChild

  instance (Functor m, Monad m) => Functor (FormT m) where
    fmap = liftM

  instance (Applicative m, Monad m) => Applicative (FormT m) where
    pure  = return
    (<*>) = ap

