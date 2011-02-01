module Control.FormPolice.FieldMap 
  ( FieldMap
  , empty
  , M.lookup
  , M.alter
  , M.null
  , insert
  ) where

  import qualified Data.Map as M

  import           Control.FormPolice.Field.Types (Field, FieldMap, empty)
  import qualified Control.FormPolice.Field as F

  insert :: Field -> FieldMap -> FieldMap
  insert field = M.alter (const $ Just field) (F.getName field)

