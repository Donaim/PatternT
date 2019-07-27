
module Util where

import Debug.Trace
import Types

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : xs) = Just x

-- | Strip all trailing zeroes
showNoZeroes :: (Show a) => a -> String
showNoZeroes x = if anydotq then striped else s
	where
		s = show x
		r = reverse s
		anydotq = any (== '.') s
		striped = reverse $ (dropWhile (== '.') . dropWhile (== '0')) r

stringifyBuiltin :: BuiltinRule -> String
stringifyBuiltin rule = case rule of
	BuiltinAdd -> "$add"
	BuiltinMultiply -> "$mult"

traceS :: (Show a) => String -> a -> a
traceS text x = trace (text ++ show x) x
