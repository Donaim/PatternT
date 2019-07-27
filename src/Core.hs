
module Core where

import Data.Maybe
import Data.Either
import Types
import Dict
import Util

type BindingDict = Dict String [Tree]

builtinReplace :: BuiltinRule -> [PatternReplacePart] -> BindingDict -> Tree
builtinReplace rule args dict = case rule of
	BuiltinAdd -> withOp (+) 0
	BuiltinMultiply -> withOp (*) 1

	where
	rargs :: [Tree]
	rargs = map (replaceWithDict dict) args

	numCastedRargs :: [Either Tree Number]
	numCastedRargs = map numcast rargs
	numcast :: Tree -> Either Tree Number
	numcast t = case treeToMaybeNum t of
		Just x -> Right x
		Nothing -> Left t

	withOp :: (Number -> Number -> Number) -> Number -> Tree
	withOp op defaul = case withOpOnMaybeNums numCastedRargs op defaul of
		[] -> numToTree defaul
		[x] -> x
		xs -> (Branch xs)

	withOpOnMaybeNums :: [Either Tree Number] -> (Number -> Number -> Number) -> Number -> [Tree]
	withOpOnMaybeNums mnums op defaul = loop Nothing mnums
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
					treeLeft = Leaf (stringifyBuiltin rule)
					treeArgs = t : withOpOnMaybeNums xs op defaul
					allArgs = case macc of
						Nothing -> treeArgs
						Just acc -> (numToTree acc) : treeArgs
					right = [Branch (treeLeft : allArgs)]

checkCond :: (Tree -> Tree) -> BindingDict -> Conditional -> Bool
checkCond simplify dict cond = case cond of
	(EqCond left right) ->
		simplify (replaceWithDict dict left)
			== simplify (replaceWithDict dict right)
	(NeqCond left right) ->
		simplify (replaceWithDict dict left)
			/= simplify (replaceWithDict dict right)
	(NotmatchCond left right) ->
		isNothing $ matchWithDict dict right $ simplify (replaceWithDict dict left)
	(LTCond left right) ->
		replaceWithDict dict left
			< replaceWithDict dict right
	(LECond left right) ->
		replaceWithDict dict left
			<= replaceWithDict dict right

matchAndReplace :: (Tree -> Tree) -> SimplifyPattern -> Tree -> Maybe Tree
matchAndReplace simplify (match, replace, conds) t =
	case matchGetDict match t of
		Nothing -> Nothing
		Just dict ->
			if all (checkCond simplify dict) conds
			then Just (replaceWithDict dict replace)
			else Nothing

replaceWithDict :: BindingDict -> PatternReplacePart -> Tree
replaceWithDict dict replace = case replace of
	(RVar token) ->
		case bindingGet dict token of
			Just t -> case t of
				[x] -> x
				xs -> Branch xs
			Nothing -> (Leaf token)
	(RBuiltin builtin args) ->
		replaceBuiltin builtin args
	(RGroup xs) ->
		replaceRgroup xs
	where
	replaceBuiltin builtin args = builtinReplace builtin args dict

	replaceRgroup xs = case loop xs of
			[x] -> x
			xs -> Branch xs
		where
		loop [] = []
		loop (r : rs) =
			case r of
				(RVar token) ->
					case bindingGet dict token of
						Just t -> case t of
							[x] -> x : loop rs
							xs -> xs ++ loop rs -- Flattening the varargs
						Nothing -> (Leaf token) : loop rs

				(RBuiltin builtin args) -> replaceBuiltin builtin args : loop rs
				(RGroup childs) -> replaceRgroup childs : loop rs

matchGetDict :: PatternMatchPart -> Tree -> Maybe BindingDict
matchGetDict match t = matchWithDict emptyDict match t

matchWithDict :: BindingDict -> PatternMatchPart -> Tree -> Maybe BindingDict
matchWithDict dict match t =
	case match of
		(Variable bindName) ->
			matchVariable dict bindName t

		(NameMatch bindName) ->
			case t of
				(Leaf symName) ->
					if bindName == symName
					then Just (bindingAdd dict bindName [t])
					else Nothing -- Names don't match
				(Branch {}) -> -- This is not a singleton branch, so we dont ever match it
					Nothing

		(VaradicMatch bindName) ->
			matchVariable dict bindName t

		(BuiltinMatch m) ->
			matchBuiltinWithDict dict m t

		(MatchGroup p ps) ->
			case t of
				(Branch xs) ->
					matchGroups dict (p : ps) xs >>= (return . bindingConcat dict)
				(Leaf x) ->
					Nothing

matchVariable :: BindingDict -> Symbol -> Tree -> Maybe BindingDict
matchVariable dict bindName t =
	case bindingGet dict bindName of
		Just [value] ->
			if t == value
			then Just (bindingAdd dict bindName [t])
			else Nothing
		(_) ->
			Just (bindingAdd dict bindName [t])

matchBuiltinWithDict :: BindingDict -> BuiltinMatchEnum -> Tree -> Maybe BindingDict
matchBuiltinWithDict dict match t = case match of
	(BuiltinMatchNumber bindName) ->
		treeToMaybeNum t >> matchVariable dict bindName t

matchGroups :: BindingDict -> [PatternMatchPart] -> [Tree] -> Maybe BindingDict
matchGroups dict [] [] = Just dict
matchGroups dict [] ts = Nothing -- Size should be equal
matchGroups dict ps [] = Nothing -- Size should be equal
matchGroups dict (p : ps) (t : ts) = case p of
	(VaradicMatch bindName) ->
		case maybeFollowingNameMatch of
			Nothing -> Just $ bindingAdd dict bindName (t : ts) ++ followingVaradictMatches

			Just nameMatch ->
				let (varadicMatched, rest) = varadicUntilName nameMatch [] (t : ts)
				in let newDict = bindingAdd dict bindName varadicMatched
					in matchGroups newDict ps rest

	(_) -> notVaradic
	where
		notVaradic = case matchWithDict dict p t of
			Nothing -> Nothing
			Just retDict ->
				let newDict = bindingConcat dict retDict
				in matchGroups newDict ps ts

		followingVaradictMatches = loop ps -- NOTE: these did not match anything
			where
			loop [] = []
			loop (x : xs) = case x of
				(VaradicMatch bindName) -> (bindName, []) : loop xs
				(_) -> loop xs

		maybeFollowingNameMatch =
			case filter isNameMatch ps of
				((NameMatch s) : rest) -> Just s
				(_) -> Nothing

		isNameMatch x = case x of
			(NameMatch {}) -> True
			(_) -> False

		varadicUntilName :: String -> [Tree] -> [Tree] -> ([Tree], [Tree])
		varadicUntilName nameMatch buf trees =
			case trees of
				[] -> break
				(t : ts) -> case t of
					(Leaf s) ->
						if s == nameMatch
						then break
						else continue
					(_) -> continue
					where continue = varadicUntilName nameMatch (t : buf) ts
			where break = (reverse buf, trees)

---------------
-- ORDERING --
---------------

compareLeafs :: Symbol -> Symbol -> Ordering
compareLeafs a b =
	case symbolToMaybeNum a of
		Nothing -> case symbolToMaybeNum b of
			Nothing -> compare a b
			Just bn -> GT
		Just an -> case symbolToMaybeNum b of
			Nothing -> LT
			Just bn -> compare an bn

instance Ord Tree where
	compare a b =
		case a of
			(Leaf as) -> case b of
				(Leaf bs) ->
					compareLeafs as bs
				(Branch {}) ->
					LT -- ASSUMPTION: no singleton branches
			(Branch xs) -> case b of
				(Leaf {}) ->
					GT -- ASSUMPTION: no singleton branches
				(Branch ys) ->
					compare (reverse xs) (reverse ys) -- NOTE: the size of branch is the secondary thing, the most important is LAST element of branch

------------------
-- APPLICATIONS --
------------------

applyTreeOne :: (Tree -> Maybe Tree) -> Tree -> Maybe Tree
applyTreeOne func t = case t of
	(Leaf s) -> func t
	(Branch childs) ->
			case loop [] childs of
				Just newme -> Just newme
				Nothing -> func t
		where
		loop :: [Tree] -> [Tree] -> Maybe Tree
		loop previus [] = Nothing
		loop previus (c : cs) = case applyTreeOne func c of
			Just newc -> Just $
				case newc of
					(Branch []) -> (Branch (previus ++ cs)) -- NOTE: erasing empty leafs!
					(Branch [x]) -> (Branch (previus ++ [x] ++ cs)) -- NOTE: erasing singletons! NOTE: the top level tree can still be a singleton, but that's ok since we will match its children anyway
					(_) -> (Branch (previus ++ [newc] ++ cs))
			Nothing -> loop (previus ++ [c]) cs

monadicApplyTreeOne :: (Monad m) =>
	(Tree -> m (Maybe (ctx, Tree))) ->
	Tree ->
	m (Maybe (ctx, Tree))
monadicApplyTreeOne func t = case t of
	(Leaf s) -> func t
	(Branch childs) -> do
		looped <- loop [] childs
		case looped of
			Just x -> return $ Just x
			Nothing -> func t
		where
		-- loop :: (Monad m) => [Tree] -> [Tree] -> m (Maybe (ctx, Tree)) -- TODO: make this type to work \=
		loop previus [] = return Nothing
		loop previus (c : cs) = do
			r <- monadicApplyTreeOne func c
			case r of
				Just (ctx, newc) -> return $ Just $ (,) ctx $
					case newc of
						(Branch []) -> (Branch (previus ++ cs)) -- NOTE: erasing empty leafs!
						(Branch [x]) -> (Branch (previus ++ [x] ++ cs)) -- NOTE: erasing singletons! NOTE: the top level tree can still be a singleton, but that's ok since we will match its children anyway
						(_) -> (Branch (previus ++ [newc] ++ cs))
				Nothing -> loop (previus ++ [c]) cs
