
module PatternT.Util where

import Text.Read (readMaybe)
import PatternT.Types

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : xs) = Just x

swapEither :: Either a b -> Either b a
swapEither e = case e of
	Left a -> Right a
	Right b -> Left b

either3 :: (a -> d) -> (b -> d) -> (c -> d) -> Either3 a b c -> d
either3 f g h me = case me of
	Left3 x -> f x
	Middle3 x -> g x
	Right3 x -> h x

partitionEither3 :: [Either3 a b c] -> ([a], [b], [c])
partitionEither3 = foldr (either3 f0 f1 f2) ([], [], [])
	 where
		f0 a ~(l0, l1, l2) = (a : l0, l1, l2)
		f1 a ~(l0, l1, l2) = (l0, a : l1, l2)
		f2 a ~(l0, l1, l2) = (l0, l1, a : l2)

replacePartToTree :: PatternReplacePart -> Tree
replacePartToTree t = case t of
	RVar x -> Leaf x
	RGroup xs -> Branch (map replacePartToTree xs)

conditionalToTrees :: Conditional -> (Tree, Tree, Tree)
conditionalToTrees c = case c of
	EqCond a b -> (replacePartToTree a, Leaf "==", replacePartToTree b)
	NeqCond a b -> (replacePartToTree a, Leaf "/=", replacePartToTree b)
	ImpliesCond a b -> (replacePartToTree a, Leaf "->", replacePartToTree b)
	LTCond a b -> (replacePartToTree a, Leaf "<", replacePartToTree b)
	LECond a b -> (replacePartToTree a, Leaf "<=", replacePartToTree b)

treeToExpr :: Tree -> Expr
treeToExpr t = case t of
	Leaf s -> Atom s
	Branch xs -> Group (map treeToExpr xs)
