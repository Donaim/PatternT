
module PatternT.Util where

import PatternT.Types

-- | Simpliest working instance of PatternElement
newtype StringyLeaf = MkStringyLeaf { unStringyLeaf :: String }
	deriving (Eq, Show, Read, Ord)

instance PatternElement StringyLeaf where
	patternElemRead = MkStringyLeaf
	patternElemShow = unStringyLeaf

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

replacePartToTree :: PatternReplacePart a -> Tree a
replacePartToTree t = case t of
	RVar x -> Leaf x
	RGroup xs -> Branch (map replacePartToTree xs)

conditionalToTrees :: (PatternElement a) => Conditional a -> (Tree a, Tree a, Tree a)
conditionalToTrees c = case c of
	EqCond a b -> (replacePartToTree a, Leaf (patternElemRead "=="), replacePartToTree b)
	NeqCond a b -> (replacePartToTree a, Leaf (patternElemRead "/="), replacePartToTree b)
	ImpliesCond a b -> (replacePartToTree a, Leaf (patternElemRead "->"), replacePartToTree b)
	LTCond a b -> (replacePartToTree a, Leaf (patternElemRead "<"), replacePartToTree b)
	LECond a b -> (replacePartToTree a, Leaf (patternElemRead "<="), replacePartToTree b)

treeToExpr :: (PatternElement a) => Tree a -> Expr
treeToExpr t = case t of
	Leaf s -> Atom (patternElemShow s) False
	Branch xs -> Group (map treeToExpr xs)

-- | Takes a list of 'a's and returns a "List" of pairs of (('a', Other 'a's that recursively got here), rec)
decreasingListsP :: [a] -> RecList (a, [a])
decreasingListsP [] = RecF []
decreasingListsP patterns = RecF continued
	where
	-- continued :: [((a, [a]), RecList (a, [a]))]
	continued = map (\ (p, ps) -> ((p, ps), decreasingListsP ps)) collected

	collected = map (\ i -> (patterns !! i, skiped i patterns)) [0 .. length patterns - 1]
	skiped skipIndex list = loop list 0
		where
		loop list i = case list of
			[] -> []
			(x : xs) -> if i == skipIndex then xs else x : loop xs (i + 1)

-- | Takes a list of 'a's and returns a "List" of pairs of ('a', rec)
-- Ex: [A, B, C] -> [A [B [C], C [B]], B [A [C], C [A]], C [A [B], B [A]]]
--     where "A [B]" is really a pair (A, [B])
decreasingLists :: [a] -> RecList a
decreasingLists [] = RecF []
decreasingLists patterns = RecF continued
	where
	continued = map (\ i -> (patterns !! i, decreasingLists $ skiped i patterns)) [0 .. length patterns - 1]
	skiped skipIndex list = loop list 0
		where
		loop list i = case list of
			[] -> []
			(x : xs) -> if i == skipIndex then xs else x : loop xs (i + 1)

showRecList :: (Show a) => RecList a -> String
showRecList (RecF []) = "[]"
showRecList (RecF (x:xs)) = "[" ++ showp x ++ (concatMap ((", " ++) . showp) xs) ++ "]"
	where
	showp (x, xs) = case xs of
		RecF [] -> show x
		xs -> show x ++ " " ++ showRecList xs
