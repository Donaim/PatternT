
module PatternT.Core where

import Data.Maybe
import Data.Either
import PatternT.Types
import PatternT.Dict

type BindingDict = Dict String [Tree]

checkCond :: [Tree -> Maybe Tree] -> BindingDict -> Conditional -> Bool
checkCond simplifies dict cond = case cond of
	(EqCond left right) ->
		simplify (replaceWithDict dict left)
			== simplify (replaceWithDict dict right)
	(NeqCond left right) ->
		simplify (replaceWithDict dict left)
			/= simplify (replaceWithDict dict right)
	(ImpliesCond left right) ->
		simplify (replaceWithDict dict left)
			== (replaceWithDict dict right)
	(LTCond left right) ->
		replaceWithDict dict left
			< replaceWithDict dict right
	(LECond left right) ->
		replaceWithDict dict left
			<= replaceWithDict dict right
	where simplify t = maybe t id (listToMaybe $ catMaybes $ map ($ t) simplifies) -- apply first simplify function, not recursive NOTE: can match a recursive builtin

matchAndReplace :: [Tree -> Maybe Tree] -> SimplifyPattern -> Tree -> Maybe Tree
matchAndReplace simplifies (match, replace, conds) t =
	case matchGetDict match t of
		Nothing -> Nothing
		Just dict ->
			if all (checkCond simplifies dict) conds
			then Just (replaceWithDict dict replace)
			else Nothing

replaceWithDict :: BindingDict -> PatternReplacePart -> Tree
replaceWithDict dict replace = case replace of
	(RVar token) ->
		case dictGet dict token of
			Just t -> case t of
				[x] -> x
				xs -> Branch xs
			Nothing -> (Leaf token)
	(RGroup xs) ->
		replaceRgroup xs
	where
	replaceRgroup xs = case loop xs of
			[x] -> x
			xs -> Branch xs
		where
		loop [] = []
		loop (r : rs) =
			case r of
				(RVar token) ->
					case dictGet dict token of
						Just vs -> vs ++ loop rs -- Flattening the varargs
						Nothing -> (Leaf token) : loop rs
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
					then Just (dictAdd dict bindName [t])
					else Nothing -- Names don't match
				(Branch {}) -> -- This is not a singleton branch, so we dont ever match it
					Nothing

		(VaradicMatch bindName) ->
			matchVariable dict bindName t

		(MatchGroup p ps) ->
			case t of
				(Branch xs) ->
					matchGroups dict (p : ps) xs
				(Leaf x) ->
					Nothing

matchVariable :: BindingDict -> Symbol -> Tree -> Maybe BindingDict
matchVariable dict bindName t =
	case dictGet dict bindName of
		Just [value] ->
			if t == value
			then Just (dictAdd dict bindName [t])
			else Nothing
		(_) ->
			Just (dictAdd dict bindName [t])

matchGroups :: BindingDict -> [PatternMatchPart] -> [Tree] -> Maybe BindingDict
matchGroups dict [] [] = Just dict
matchGroups dict [] ts = Nothing -- Size should be equal
matchGroups dict ps [] = Nothing -- Size should be equal
matchGroups dict (p : ps) (t : ts) = case p of
	(VaradicMatch bindName) ->
		case followingExactMatches of
			[] -> withVaradics bindName (t : ts) []
			ms -> let (varadicMatched, rest) = varadicUntilExact ms [] (t : ts)
				in withVaradics bindName varadicMatched rest

	(_) -> notVaradic
	where
		withVaradics bindName varargs rest =
			if variableDiff < 0
			then Nothing
			else case followingExactMatches of
				[] -> Just $ dictAdd dictWithVariables bindName dropedVariables ++ followingVaradictMatches
				ms -> let newDict = dictAdd dictWithVariables bindName dropedVariables
					in matchGroups newDict ps rest

			where
			variableDiff = length varargs - length followingVariableMatches
			dropedVariables = take variableDiff varargs -- Substructing variables from the end
			takenVariables = zip followingVariableMatches (map (\ x -> [x]) $ drop variableDiff varargs)
			dictWithVariables = dict ++ takenVariables

		notVaradic = case matchWithDict dict p t of
			Nothing -> Nothing
			Just retDict ->
				let newDict = dictConcat dict retDict
				in matchGroups newDict ps ts

		followingVaradictMatches = loop ps -- NOTE: these did not match anything
			where
			loop [] = []
			loop (x : xs) = case x of
				(VaradicMatch bindName) -> (bindName, []) : loop xs
				(_) -> loop xs

		(followingVariableMatches, followingExactMatches) = partitionEithers $ loop False ps
			where
			loop foundQ patterns = case patterns of
				[] -> []
				(x : xs) -> case x of
					(NameMatch {}) -> Right x : loop True xs
					(MatchGroup {}) -> Right x : loop True xs
					(Variable bindName) -> if foundQ then [] else Left bindName : loop False xs
					(_) -> if foundQ then [] else loop False xs

		varadicUntilExact :: [PatternMatchPart] -> [Tree] -> [Tree] -> ([Tree], [Tree])
		varadicUntilExact matches buf trees =
			case trees of
				[] -> break
				(t : ts) -> if matchSome trees matches
					then break
					else varadicUntilExact matches (t : buf) ts
			where break = (reverse buf, trees)

		matchSome :: [Tree] -> [PatternMatchPart] -> Bool
		matchSome trees patterns = case patterns of
			[] -> True
			(p : ps) -> case trees of
				[] -> False
				(t : ts) -> case matchGetDict p t of
					Just {} -> matchSome ts ps
					Nothing -> False

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

--------------
-- ORDERING --
--------------

instance Ord Tree where
	compare a b =
		case a of
			(Leaf as) -> case b of
				(Leaf bs) ->
					compare as bs
				(Branch {}) ->
					LT -- ASSUMPTION: no singleton branches
			(Branch xs) -> case b of
				(Leaf {}) ->
					GT -- ASSUMPTION: no singleton branches
				(Branch ys) ->
					compare xs ys -- NOTE: the size of branch is the secondary thing, the most important is first element of branch
