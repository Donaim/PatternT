
-- | Interface with simplification procedures
module PatternT.SimplifyInterface where

import Data.Either
import Data.Maybe

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
applyFirstSimplification :: [Tree -> Maybe Tree] -> [SimplifyPattern] -> Tree -> Maybe (Tree, SimplifyPattern)
applyFirstSimplification simplifies patterns t0 = loop patterns t0
	where
	loop patterns t = case patterns of
		[] -> Nothing
		(x : xs) -> case applyTreeOne (matchAndReplace simplifies x) t of
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
	[(SimplificationF m ctx, [Tree -> Maybe Tree])] ->
	ctx ->
	Tree ->
	m (Maybe (Tree, Either SimplifyPattern String, ctx))
mixedApplyFirstSimplificationWithSimplify simplifications ctx t0 = loop simplifications t0
	where
	-- loop :: [SimplificationF m ctx] -> Tree -> m (Maybe (Tree, Either SimplifyPattern String, ctx))
	loop simplifications t = case simplifications of
		[] -> return Nothing
		((simpl, condSimpl) : xs) -> case simpl of
			Left3 pattern ->
				let r = applyTreeOne (matchAndReplace condSimpl pattern) t
				in case r of
					Just newt -> return $ Just (newt, Left pattern, ctx)
					Nothing -> loop xs t

			Middle3 (name, func) -> do
				r <- monadicApplyTreeOne (monadicMatchAndReplace name (func condSimpl ctx)) t
				case r of
					Just (newCtx, newt) -> return $ Just (newt, Right name, newCtx)
					Nothing -> loop xs t

			Right3 (name, func) ->
				let r = applyTreeOne (withFunctionNameCheck Nothing (name, func condSimpl)) t
				in case r of
					Just newt -> return $ Just (newt, Right name, ctx)
					Nothing -> loop xs t

-----------
-- LOOPS --
-----------

applySimplificationsUntil0Debug :: [SimplifyPattern] -> Tree -> [(Tree, SimplifyPattern)]
applySimplificationsUntil0Debug patterns t0 = loop t0
	where
	simplifies = makeSimplifies patterns
	loop t = case applyFirstSimplification simplifies patterns t of
		Nothing -> []
		Just (newt, rule) -> (newt, rule) : loop newt

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
		r <- mixedApplyFirstSimplificationWithSimplify simplifies ctx t
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

-- | Using mixed rules, take pure ones and make simplify functions for each rule from them to use in Conditionals
-- This method has 2 features:
--  * accepts optional "limit" value which makes it so that conditionals can be nested "limit" of times only
--  * returned simplifications don't contain patterns that match their own conditionals. Ex: "True -> False | True" will not result in infinite recursion caused by conditionals simplifications. NOTE: unbound recursion can still be achieved if there is cyclic Conditional dependencies, ex: "1) p0 x -> 0 | p1 a ; 2) p1 x -> 1 | p0 x", but this is preventible by limit
makeSimplifiesFromMixed :: (Monad m) => Maybe Int -> [SimplificationF m ctx] -> [(SimplificationF m ctx, [Tree -> Maybe Tree])]
makeSimplifiesFromMixed condRecLimit simplifications = maybe (mapwithcur unlimited) (\ l -> map (\ s -> (s, limited s l 0)) simplifications) condRecLimit
	where
	-- mapwithcur :: (SimplificationF m ctx -> [Tree -> Maybe Tree]) -> [(SimplificationF m ctx, [Tree -> Maybe Tree])]
	mapwithcur f = map (\ s -> (s, f s)) simplifications

	limited :: SimplificationF m ctx -> Int -> Int -> [Tree -> Maybe Tree]
	limited current limit = let f n = if n >= limit then [] else withrec current (f (n + 1)) in f

	unlimited :: SimplificationF m ctx -> [Tree -> Maybe Tree]
	unlimited current = let f = withrec current f in f

	withrec :: SimplificationF m ctx -> [Tree -> Maybe Tree] -> [Tree -> Maybe Tree]
	withrec current firstAggregated = collectSimplify simplifications
		where
		applyPattern :: SimplifyPattern -> (Tree -> Maybe Tree)
		applyPattern (match, replace, conds) =
			if anymatched -- If any conditional gets matched by the match part of pattern, reduction usually results in infinite recursion, so skip that pattern
			then const Nothing
			else matchAndReplace firstAggregated (match, replace, conds)
			where
			condTrees = foldl (\ acc (left, s, right) -> left : right : acc) [] $ map conditionalToTrees conds
			anymatched = any isJust $ map (matchGetDict match) condTrees

		collectSimplify :: [SimplificationF m ctx] -> [(Tree -> Maybe Tree)]
		collectSimplify [] = []
		collectSimplify (f : fs) = case f of
			Left3 pattern -> (applyPattern pattern) : collectSimplify fs
			Middle3 {} -> collectSimplify fs
			Right3 (name, func) -> (withFunctionNameCheck Nothing (name, func firstAggregated)) : collectSimplify fs

makeSimplifies :: [SimplifyPattern] -> [Tree -> Maybe Tree]
makeSimplifies patterns = let f = map (matchAndReplace f) patterns in f
	-- TODO: skip patterns that match own conditionals. Based on how it's done in `makeSimplifiesFromMixed'
