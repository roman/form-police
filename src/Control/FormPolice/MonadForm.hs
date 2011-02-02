module Control.FormPolice.MonadForm where

  import           Data.Text (Text)
  import           Data.Aeson (ToJSON, FromJSON)
  import           Data.Monoid (Monoid)
  
  class (Monad m) => MonadForm m where
    appendError   :: Text -> m ()
    getValue :: (Monoid a, FromJSON a) => m a

    text     :: (Monoid a, ToJSON a, FromJSON a) => Text -> m a 
    password :: (Monoid a, ToJSON a, FromJSON a) => Text -> m a
    textarea :: (Monoid a, ToJSON a, FromJSON a) => Text -> m a
    checkbox :: (Monoid a, ToJSON a, FromJSON a) => Text -> m a
    radio    :: (Monoid a, ToJSON a, FromJSON a) => Text -> [(Text, Text)] -> m a
    select   :: (Monoid a, ToJSON a, FromJSON a) => Text -> [(Text, Text)] -> m a
    child    :: (Monoid a, ToJSON a, FromJSON a) => Text -> m a -> m a
    
