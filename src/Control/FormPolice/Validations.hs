module Control.FormPolice.Validations where

  import           Data.Text (Text)
  import           Data.Aeson (FromJSON)
  import           Data.Monoid (Monoid)

  import           Control.Monad (unless, forM_)

  import           Control.FormPolice.MonadForm

  validates :: MonadForm m => m a -> [a -> m ()] -> m a
  validates action vs = do
    a <- action
    forM_ vs $ \fn -> fn a
    return a

  validate :: (MonadForm m, Monoid a, FromJSON a) => (a -> Bool) -> Text -> a -> m ()
  validate pn errorMsg value = unless (pn value) (appendError errorMsg)
    
