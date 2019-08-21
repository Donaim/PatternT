
-- | Interface with simplification procedures
module PatternT.SimplifyInterface where

import Data.Either

import PatternT.Types
import PatternT.Core
import PatternT.Util

----------
-- PURE --
----------

applySimplifications :: [SimplifyPattern] -> Tree -> [Tree]
applySimplifications patterns t0 = loop patterns t0
	where
	simplify = makeSimplifies patterns
	loop patterns t = case patterns of
		[] -> []
		(x : xs) -> case applyTreeOne (matchAndReplace simplify x) t of
			Just newt -> newt : (loop xs newt)
			Nothing -> loop xs t

-- Breaks on first success
applyFirstSimplification :: [SimplifyPattern] -> Tree -> Maybe (Tree, SimplifyPattern)
applyFirstSimplification patterns t0 = loop patterns t0
	where
	simplify = makeSimplifies patterns
	loop patterns t = case patterns of
		[] -> Nothing
		(x : xs) -> case applyTreeOne (matchAndReplace simplify x) t of
			Just newt -> Just (newt, x)
			Nothing -> loop xs t

-- Breaks on first success, does not return SimplifyPattern that matched
applyFirstSimplificationL :: [SimplifyPattern] -> Tree -> Maybe Tree
applyFirstSimplificationL patterns t0 = loop patterns t0
	where
	simplify = makeSimplifies patterns
	loop patterns t = case patterns of
		[] -> Nothing
		(x : xs) -> case applyTreeOne (matchAndReplace simplify x) t of
			Just newt -> Just newt
			Nothing -> loop xs t

applyFirstSimplificationF :: [Tree -> Maybe Tree] -> Tree -> Maybe Tree
applyFirstSimplificationF funcs t0 = loop funcs t0
	where
	loop funcs t = case funcs of
		[] -> Nothing
		(f : fs) -> case f t of
			Just newt -> Just newt
			Nothing -> loop fs t

applySimplificationsUntil0LastF :: (Tree -> Maybe Tree) -> Tree -> Tree
applySimplificationsUntil0LastF func t0 = loop t0
	where
	loop t = case func t of
		Nothing -> t
		Just newt -> loop newt

applySimplificationsUntil0Last :: [SimplifyPattern] -> Tree -> Tree
applySimplificationsUntil0Last patterns t0 = loop t0
	where
	loop t = case applyFirstSimplificationL patterns t of
		Nothing -> t
		Just newt -> loop newt

-----------------------------
-- MONADIC SIMPLIFICATIONS --
-----------------------------

monadicMatchAndReplace :: (Monad m) =>
	String ->
	(Tree -> m (Maybe (ctx, Tree))) ->
	Tree ->
	m (Maybe (ctx, Tree))
monadicMatchAndReplace name func =
	withFunctionNameCheck (return Nothing) (name, func)

monadicApplyFirstSimplification :: (Monad m) =>
	[Tree -> Maybe Tree] ->
	[MonadicSimplify m ctx] ->
	ctx ->
	Tree ->
	m (Maybe (Tree, String, ctx))
monadicApplyFirstSimplification simplify simplifications ctx t0 = loop simplifications t0
	where
	loop simplifications t = case simplifications of
		[] -> return Nothing
		((name, func) : xs) -> do
			r <- monadicApplyTreeOne (monadicMatchAndReplace name (func simplify ctx)) t
			case r of
				Just (newCtx, newt) -> return $ Just (newt, name, newCtx)
				Nothing -> loop xs t

-----------
-- MIXED --
-----------

mixedApplyFirstSimplificationWithSimplify :: (Monad m) =>
	[Tree -> Maybe Tree] ->
	[SimplificationF m ctx] ->
	ctx ->
	Tree ->
	m (Maybe (Tree, Either SimplifyPattern String, ctx))
mixedApplyFirstSimplificationWithSimplify simplify simplifications ctx t0 = loop simplifications t0
	where
	-- loop :: [SimplificationF m ctx] -> Tree -> m (Maybe (Tree, Either SimplifyPattern String, ctx))
	loop simplifications t = case simplifications of
		[] -> return Nothing
		(simpl : xs) -> case simpl of
			Left3 pattern ->
				let r = applyTreeOne (matchAndReplace simplify pattern) t
				in case r of
					Just newt -> return $ Just (newt, Left pattern, ctx)
					Nothing -> loop xs t

			Middle3 (name, func) -> do
				r <- monadicApplyTreeOne (monadicMatchAndReplace name (func simplify ctx)) t
				case r of
					Just (newCtx, newt) -> return $ Just (newt, Right name, newCtx)
					Nothing -> loop xs t

			Right3 (name, func) ->
				let r = applyTreeOne (withFunctionNameCheck Nothing (name, func simplify)) t
				in case r of
					Just newt -> return $ Just (newt, Right name, ctx)
					Nothing -> loop xs t

-----------
-- LOOPS --
-----------

applySimplificationsUntil0Debug :: [SimplifyPattern] -> Tree -> [(Tree, SimplifyPattern)]
applySimplificationsUntil0Debug patterns0 t0 = loop patterns0 t0
	where
	loop patterns t = case applyFirstSimplification patterns t of
		Nothing -> []
		Just (newt, rule) -> (newt, rule) : loop patterns newt

mixedApplySimplificationsUntil0Debug :: (Monad m) =>
	Maybe Int ->
	[SimplificationF m ctx] ->
	ctx ->
	Tree ->
	m [(Tree, Either SimplifyPattern String, ctx)]
mixedApplySimplificationsUntil0Debug condRecLimit simplifications ctx0 t0 = loop ctx0 t0
	where
	simplifies = makeSimplifiesFromMixed condRecLimit simplifications
	loop ctx t = do
		r <- mixedApplyFirstSimplificationWithSimplify simplifies simplifications ctx t
		case r of
			Nothing -> return []
			Just (newt, rule, newCtx) -> do
				next <- loop newCtx newt
				return $ (newt, rule, newCtx) : next

-----------
-- UTILS --
-----------

withFunctionNameCheck :: b -> (String, Tree -> b) -> (Tree -> b)
withFunctionNameCheck defaul (name, func) tree = case tree of -- NOTE: in simplify function we always check the name!
	(Leaf s) ->
		if s == name
		then func tree
		else defaul
	(Branch (x : xs)) ->
		case x of
			(Leaf s) ->
				if s == name
				then func tree
				else defaul
			(_) -> defaul
	(_) -> defaul

-- | Using mixed rules, take pure ones and make simplify functions from them to use in Conditionals
makeSimplifiesFromMixed :: (Monad m) => Maybe Int -> [SimplificationF m ctx] -> [Tree -> Maybe Tree]
makeSimplifiesFromMixed condRecLimit simplifications = maybe unlimited (limited 0) condRecLimit
	where
	limited :: Int -> Int -> [Tree -> Maybe Tree]
	limited n limit = if n >= limit then [] else withrec (limited (n + 1) limit)

	unlimited :: [Tree -> Maybe Tree]
	unlimited = withrec unlimited

	withrec firstAggregated = collectSimplify simplifications
		where
		applyPattern :: SimplifyPattern -> (Tree -> Maybe Tree)
		applyPattern pattern = matchAndReplace firstAggregated pattern

		collectSimplify :: [SimplificationF m ctx] -> [(Tree -> Maybe Tree)]
		collectSimplify [] = []
		collectSimplify (f : fs) = case f of
			Left3 pattern -> (applyPattern pattern) : collectSimplify fs
			Middle3 {} -> collectSimplify fs
			Right3 (name, func) -> (withFunctionNameCheck Nothing (name, func firstAggregated)) : collectSimplify fs

makeSimplifies :: [SimplifyPattern] -> [Tree -> Maybe Tree]
makeSimplifies patterns = let f = map (matchAndReplace f) patterns in f
