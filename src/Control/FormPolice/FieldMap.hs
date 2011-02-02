module Control.FormPolice.FieldMap 
  ( FieldMap
  , empty
  , M.lookup
  , M.alter
  , M.null
  , insert
  , mapErrors
  ) where

  import           Data.Text (Text)

  import qualified Data.Map as M

  import           Control.Monad ((>=>))

  import           Control.FormPolice.Field.Types (Field, FieldMap, empty)
  import qualified Control.FormPolice.Field as F

  insert :: Field -> FieldMap -> FieldMap
  insert field = M.alter (const $ Just field) (F.getName field)

  mapErrors :: (Text -> [Text] -> b) -> FieldMap -> [b]
  mapErrors fn = M.foldWithKey helper []
    where
      helper k v accum = (fn k (F.getErrors v) : accum)

