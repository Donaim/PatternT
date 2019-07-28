
module PatternT.Util where

import Text.Read (readMaybe)
import Debug.Trace
import PatternT.Types

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : xs) = Just x

swapEither :: Either a b -> Either b a
swapEither e = case e of
	Left a -> Right a
	Right b -> Left b

-- | Strip all trailing zeroes
showNoZeroes :: (Show a) => a -> String
showNoZeroes x = if anydotq then striped else s
	where
		s = show x
		r = reverse s
		anydotq = any (== '.') s
		striped = reverse $ (dropWhile (== '.') . dropWhile (== '0')) r

numToTree :: Number -> Tree
numToTree x = Leaf (showNoZeroes x)

symbolToMaybeNum :: Symbol -> Maybe Number
symbolToMaybeNum s = case readMaybe s :: Maybe Number of
	Just x -> Just x
	Nothing -> Nothing

treeToMaybeNum :: Tree -> Maybe Number
treeToMaybeNum t = case t of
	(Leaf s) -> symbolToMaybeNum s
	(Branch {}) -> Nothing

traceS :: (Show a) => String -> a -> a
traceS text x = trace (text ++ show x) x

either3 :: (a -> d) -> (b -> d) -> (c -> d) -> Tuple3 a b c -> d
either3 f g h me = case me of
	Tuple30 x -> f x
	Tuple31 x -> g x
	Tuple32 x -> h x

partitionTuple3 :: [Tuple3 a b c] -> ([a], [b], [c])
partitionTuple3 = foldr (either3 f0 f1 f2) ([], [], [])
	 where
		f0 a ~(l0, l1, l2) = (a : l0, l1, l2)
		f1 a ~(l0, l1, l2) = (l0, a : l1, l2)
		f2 a ~(l0, l1, l2) = (l0, l1, a : l2)
