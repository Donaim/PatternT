
-- | Ready to use monadic simplification rules
module PatternT.MonadicRules where

import PatternT.Types
import PatternT.Util
import PatternT.SimplifyInterface

ruleAdd :: String -> Tree -> Maybe Tree
ruleAdd = stdNumberRule (+)

ruleMult :: String -> Tree -> Maybe Tree
ruleMult = stdNumberRule (*)

ruleSub :: String -> Tree -> Maybe Tree
ruleSub = stdNumberRule (-)

ruleDiv :: String -> Tree -> Maybe Tree
ruleDiv = stdNumberRule (/)

rulePow :: String -> Tree -> Maybe Tree
rulePow = stdNumberRule rationalPow

-----------
-- UTILS --
-----------

rationalPow :: Number -> Number -> Number
rationalPow a b = toRational $ (fromRational a) ** (fromRational b)

stdNumberRule :: (Number -> Number -> Number) -> String -> Tree -> Maybe Tree
stdNumberRule op name t = case t of
	Leaf x -> Nothing
	(Branch []) -> Nothing
	(Branch (x : rargs)) -> -- ASSUMPTION: x == name
		differentOrNothing failcase $ withOp op failcase rargs
	where failcase = Leaf name

differentOrNothing :: Tree -> Tree -> Maybe Tree
differentOrNothing failcase t = case t of
	(Branch (x : xs)) ->
		if x == failcase
		then Nothing
		else Just t
	(_) -> Just t

numCast :: [Tree] -> [Either Tree Number]
numCast = map mapf
	where
	mapf t = case treeToMaybeNum t of
		Just x -> Right x
		Nothing -> Left t

withOp :: (Number -> Number -> Number) -> Tree -> [Tree] -> Tree
withOp op failcase rargs = case withOpOnMaybeNums op failcase numcasted of
	[] -> failcase
	[x] -> x
	xs -> (Branch xs)
	where numcasted = numCast rargs

withOpOnMaybeNums :: (Number -> Number -> Number) -> Tree -> [Either Tree Number] -> [Tree]
withOpOnMaybeNums op failcase mnums = loop Nothing mnums
	where
	loop :: Maybe Number -> [Either Tree Number] -> [Tree]
	loop macc [] = case macc of
		Nothing -> []
		Just acc -> [numToTree acc]
	loop macc (x : xs) =
		case x of
			Right num ->
				case macc of
					Just acc -> loop (Just $ op acc num) xs
					Nothing -> loop (Just num) xs
			Left t -> right
				where
				treeArgs = t : withOpOnMaybeNums op failcase xs
				allArgs = case macc of
					Nothing -> treeArgs
					Just acc -> (numToTree acc) : treeArgs
				right = [Branch (failcase : allArgs)]


