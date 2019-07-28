
-- | Ready to use monadic simplification rules
module MonadicRules where

import Types
import Util
import SimplifyInterface

ruleAdd :: String -> Tree -> Maybe Tree
ruleAdd name t = case t of
	Leaf x -> Nothing
	(Branch []) -> Nothing
	(Branch (x : rargs)) -> -- ASSUMPTION: x == name
		differentOrNothing failcase $ withOp (+) 0 failcase rargs
	where failcase = Leaf name

ruleMult :: String -> Tree -> Maybe Tree
ruleMult name t = case t of
	Leaf x -> Nothing
	(Branch []) -> Nothing
	(Branch (x : rargs)) -> -- ASSUMPTION: x == name
		differentOrNothing failcase $ withOp (*) 1 failcase rargs
	where failcase = Leaf name

-----------
-- UTILS --
-----------

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

withOp :: (Number -> Number -> Number) -> Number -> Tree -> [Tree] -> Tree
withOp op defaul failcase rargs = case withOpOnMaybeNums op defaul failcase numcasted of
	[] -> numToTree defaul
	[x] -> x
	xs -> (Branch xs)
	where numcasted = numCast rargs

withOpOnMaybeNums :: (Number -> Number -> Number) -> Number -> Tree -> [Either Tree Number] -> [Tree]
withOpOnMaybeNums op defaul failcase mnums = loop Nothing mnums
	where
	loop :: Maybe Number -> [Either Tree Number] -> [Tree]
	loop macc [] = case macc of
		Nothing -> []
		Just acc -> [numToTree acc]
	loop macc (x : xs) =
		case x of
			Right num ->
				let newacc = case macc of
					Just acc -> op acc num
					Nothing -> op defaul num
				in loop (Just newacc) xs
			Left t -> right
				where
				treeArgs = t : withOpOnMaybeNums op defaul failcase xs
				allArgs = case macc of
					Nothing -> treeArgs
					Just acc -> (numToTree acc) : treeArgs
				right = [Branch (failcase : allArgs)]


