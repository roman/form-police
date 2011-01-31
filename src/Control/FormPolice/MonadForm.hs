module Control.FormPolice.MonadForm where

  import           Data.Text (Text)
  import           Data.Aeson (FromJSON)
  import           Data.Monoid (Monoid)
  
  class (Monad m) => MonadForm m where
    text     :: (Monoid a, FromJSON a) => Text -> m a 
    password :: (Monoid a, FromJSON a) => Text -> m a
    textarea :: (Monoid a, FromJSON a) => Text -> m a
    checkbox :: (Monoid a, FromJSON a) => Text -> m a
    radio    :: (Monoid a, FromJSON a) => Text -> [(Text, Text)] -> m a
    select   :: (Monoid a, FromJSON a) => Text -> [(Text, Text)] -> m a
    child    :: (Monoid a, FromJSON a) => Text -> m a -> m a
    
