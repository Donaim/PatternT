
module PatternT.Core where

import Data.Maybe
import Data.Either
import PatternT.Types
import PatternT.Dict

type BindingDict a = Dict a [Tree a]

checkCond :: (PatternElement a) => [Tree a -> Maybe (Tree a)] -> BindingDict a -> Conditional a -> Bool
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

matchAndReplace :: (PatternElement a) => [Tree a -> Maybe (Tree a)] -> PatternMatchPart a -> PatternReplacePart a -> [Conditional a] -> Tree a -> Maybe (Tree a)
matchAndReplace simplifies match replace conds t =
	case matchGetDict match t of
		Nothing -> Nothing
		Just dict ->
			if all (checkCond simplifies dict) conds
			then Just (replaceWithDict dict replace)
			else Nothing

replaceWithDict :: (PatternElement a) => BindingDict a -> PatternReplacePart a -> Tree a
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

matchGetDict :: (PatternElement a) => PatternMatchPart a -> Tree a -> Maybe (BindingDict a)
matchGetDict match t = matchWithDict emptyDict match t

matchWithDict :: (PatternElement a) => BindingDict a -> PatternMatchPart a -> Tree a -> Maybe (BindingDict a)
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

matchVariable :: (PatternElement a) => BindingDict a -> a -> Tree a -> Maybe (BindingDict a)
matchVariable dict bindName t =
	case dictGet dict bindName of
		Just [value] ->
			if t == value
			then Just (dictAdd dict bindName [t])
			else Nothing
		(_) ->
			Just (dictAdd dict bindName [t])

matchGroups :: (PatternElement a) => BindingDict a -> [PatternMatchPart a] -> [Tree a] -> Maybe (BindingDict a)
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

		varadicUntilExact :: (PatternElement a) => [PatternMatchPart a] -> [Tree a] -> [Tree a] -> ([Tree a], [Tree a])
		varadicUntilExact matches buf trees =
			case trees of
				[] -> break
				(t : ts) -> if matchSome trees matches
					then break
					else varadicUntilExact matches (t : buf) ts
			where break = (reverse buf, trees)

		matchSome :: (PatternElement a) => [Tree a] -> [PatternMatchPart a] -> Bool
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

-- | Bottom-up apply first
applyTreeOne :: (PatternElement a) => (Tree a -> Maybe (Tree a)) -> Tree a -> Maybe (Tree a)
applyTreeOne func t = case t of
	(Leaf s) -> func t
	(Branch childs) ->
			case loop [] childs of
				Just newme -> Just newme
				Nothing -> func t
		where
		-- loop :: (PatternElement a) => [Tree a] -> [Tree a] -> Maybe (Tree a)
		loop previus [] = Nothing
		loop previus (c : cs) = case applyTreeOne func c of
			Just newc -> Just $
				case newc of
					(Branch []) -> (Branch (previus ++ cs)) -- NOTE: erasing empty leafs!
					(Branch [x]) -> (Branch (previus ++ [x] ++ cs)) -- NOTE: erasing singletons! NOTE: the top level tree can still be a singleton, but that's ok since we will match its children anyway
					(_) -> (Branch (previus ++ [newc] ++ cs))
			Nothing -> loop (previus ++ [c]) cs

monadicApplyTreeOne :: (PatternElement a) => (Monad m) =>
	(Tree a -> m (Maybe (ctx, Tree a))) ->
	Tree a ->
	m (Maybe (ctx, Tree a))
monadicApplyTreeOne func t = case t of
	(Leaf s) -> func t
	(Branch childs) -> do
		looped <- loop [] childs
		case looped of
			Just x -> return $ Just x
			Nothing -> func t
		where
		-- loop :: (PatternElement a) => (Monad m) => [Tree a] -> [Tree a] -> m (Maybe (ctx, Tree a)) -- TODO: make this type to work \=
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

-- | Top-down apply all
applyTreeAll :: (PatternElement a) => (Tree a -> Maybe (Tree a)) -> Tree a -> Maybe (Tree a)
applyTreeAll func t = case t of
	(Leaf s) -> func t
	(Branch oldchilds) -> case func t of
		Just newme -> Just $ case newme of
			Leaf leafme -> newme
			Branch newchilds -> case getChilds newchilds of
				Just bestchilds -> Branch bestchilds
				Nothing -> newme
		Nothing -> case getChilds oldchilds of
			Just bestchilds -> Just (Branch bestchilds)
			Nothing -> Nothing
		where
		getChilds = loop False []

		-- loop :: (PatternElement a) => Bool -> [Tree a] -> [Tree a] -> Maybe (Tree a)
		loop changed previus [] = if changed then Just (reverse previus) else Nothing
		loop changed previus (c : cs) = case applyTreeAll func c of
			Just newc -> case newc of
				(Branch []) -> loop True previus cs -- NOTE: erasing empty leafs!
				(Branch [x]) -> loop True (x : previus) cs -- NOTE: erasing singletons! NOTE: the top level tree can still be a singleton, but that's ok since we will match its children anyway
				(_) -> loop True (newc : previus) cs
			Nothing -> loop changed (c : previus) cs
