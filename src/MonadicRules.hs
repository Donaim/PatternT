
-- | Ready to use monadic simplification rules
module MonadicRules where

import Types
import Util

monadicRuleAdd :: (Monad m) => MonadicSimplify m ctx
monadicRuleAdd = (name, func)
	where
	name = "$add"
	func ctx t = return $ case ruleAdd name t of
		Nothing -> Nothing
		Just newt -> Just (ctx, newt)

monadicRuleMult :: (Monad m) => MonadicSimplify m ctx
monadicRuleMult = (name, func)
	where
	name = "$mult"
	func ctx t = return $ case ruleMult name t of
		Nothing -> Nothing
		Just newt -> Just (ctx, newt)


ruleAdd :: String -> Tree -> Maybe Tree
ruleAdd name t = case t of
	Leaf x -> Nothing
	(Branch []) -> Nothing
	(Branch (x : rargs)) -> -- ASSUMPTION: x == name
		Just $ withOp (+) 0 (Leaf name) rargs

ruleMult :: String -> Tree -> Maybe Tree
ruleMult name t = case t of
	Leaf x -> Nothing
	(Branch []) -> Nothing
	(Branch (x : rargs)) -> -- ASSUMPTION: x == name
		Just $ withOp (*) 1 (Leaf name) rargs

-----------
-- UTILS --
-----------

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

