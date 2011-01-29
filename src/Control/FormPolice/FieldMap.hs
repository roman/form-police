module Control.FormPolice.FieldMap 
  ( FieldMap
  , M.empty
  , M.lookup
  , insert
  ) where

  import           Data.Text (Text)

  import           Data.Map (Map)
  import qualified Data.Map as M

  import           Control.FormPolice.Field (Field)
  import qualified Control.FormPolice.Field as F

  type FieldMap = Map Text Field

  insert :: Field -> FieldMap -> FieldMap
  insert field = M.alter (const $ Just field) (F.getName field)

