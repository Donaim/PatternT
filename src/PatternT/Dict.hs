
-- | Dictionary to use for pattern matching
module PatternT.Dict where

import Data.List
import PatternT.Types

type Dict k v = [(k, v)]

emptyDict :: Dict k x
emptyDict = []

bindingGet :: (Eq k) => Dict k v -> k -> Maybe v
bindingGet dict key =
	case find ((== key) . fst) dict of
		Nothing -> Nothing
		Just (k, v) -> Just v

bindingAdd :: Dict k v -> k -> v -> Dict k v
bindingAdd dict key value = (key, value) : dict

bindingConcat :: Dict k v -> Dict k v -> Dict k v
bindingConcat a b = a ++ b


