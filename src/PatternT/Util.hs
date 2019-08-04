
module PatternT.Util where

import Text.Read (readMaybe)
import PatternT.Types

maybeDefault :: a -> (a -> Maybe a) -> a
maybeDefault x f = case f x of
	Just y -> y
	Nothing -> x

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : xs) = Just x

swapEither :: Either a b -> Either b a
swapEither e = case e of
	Left a -> Right a
	Right b -> Left b

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
