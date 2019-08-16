
-- | Dictionary to use for pattern matching
module PatternT.Dict where

import Data.List

type Dict k v = [(k, v)]

emptyDict :: Dict k x
emptyDict = []

dictGet :: (Eq k) => Dict k v -> k -> Maybe v
dictGet dict key =
	case find ((== key) . fst) dict of
		Nothing -> Nothing
		Just (k, v) -> Just v

dictAdd :: Dict k v -> k -> v -> Dict k v
dictAdd dict key value = (key, value) : dict

dictConcat :: Dict k v -> Dict k v -> Dict k v
dictConcat a b = a ++ b


