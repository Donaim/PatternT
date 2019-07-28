
-- | Interface with simplification procedures
module SimplifyInterface where

import Data.Either

import Types
import Core
import Util

----------
-- PURE --
----------

applySimplifications :: [SimplifyPattern] -> Tree -> [Tree]
applySimplifications patterns t0 = loop patterns t0
	where
	simplify = applySimplificationsUntil0Last patterns
	loop patterns t = case patterns of
		[] -> []
		(x : xs) -> case applyTreeOne (matchAndReplace simplify x) t of
			Just newt -> newt : (loop xs newt)
			Nothing -> loop xs t

-- Breaks on first success
applyFirstSimplification :: [SimplifyPattern] -> Tree -> Maybe (Tree, SimplifyPattern)
applyFirstSimplification patterns t0 = loop patterns t0
	where
	simplify = applySimplificationsUntil0Last patterns
	loop patterns t = case patterns of
		[] -> Nothing
		(x : xs) -> case applyTreeOne (matchAndReplace simplify x) t of
			Just newt -> Just (newt, x)
			Nothing -> loop xs t

applyFirstSimplificationF :: [Tree -> Maybe Tree] -> Tree -> Maybe Tree
applyFirstSimplificationF funcs t0 = loop funcs t0
	where
	loop funcs t = case funcs of
		[] -> Nothing
		(f : fs) -> case f t of
			Just newt -> Just newt
			Nothing -> loop fs t

applySimplificationsUntil0Last :: [SimplifyPattern] -> Tree -> Tree
applySimplificationsUntil0Last patterns0 t0 = loop patterns0 t0
	where
	loop patterns t = case applyFirstSimplification patterns t of
		Nothing -> t
		Just newt -> loop patterns (fst newt)

applySimplificationsUntil0LastF :: (Tree -> Maybe Tree) -> Tree -> Tree
applySimplificationsUntil0LastF func t0 = loop t0
	where
	loop t = case func t of
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
	[MonadicSimplify m ctx] ->
	ctx ->
	Tree ->
	m (Maybe (Tree, String, ctx))
monadicApplyFirstSimplification simplifications ctx t0 = loop simplifications t0
	where
	loop simplifications t = case simplifications of
		[] -> return Nothing
		((name, func) : xs) -> do
			r <- monadicApplyTreeOne (monadicMatchAndReplace name (func ctx)) t
			case r of
				Just (newCtx, newt) -> return $ Just (newt, name, newCtx)
				Nothing -> loop xs t

-----------
-- MIXED --
-----------

mixedApplyFirstSimplificationWithSimplify :: (Monad m) =>
	(Tree -> Tree) ->
	[SimplificationF m ctx] ->
	ctx ->
	Tree ->
	m (Maybe (Tree, Either SimplifyPattern String, ctx))
mixedApplyFirstSimplificationWithSimplify simplify simplifications ctx t0 = loop simplifications t0
	where
	loop simplifications t = case simplifications of
		[] -> return Nothing
		(simpl : xs) -> case simpl of
				Tuple30 pattern ->
					let r = applyTreeOne (matchAndReplace simplify pattern) t
					in case r of
						Just newt -> return $ Just (newt, Left pattern, ctx)
						Nothing -> loop xs t

				Tuple31 (name, func) -> do
					r <- monadicApplyTreeOne (monadicMatchAndReplace name (func ctx)) t
					case r of
						Just (newCtx, newt) -> return $ Just (newt, Right name, newCtx)
						Nothing -> loop xs t

				Tuple32 pure ->
					let r = applyTreeOne (withFunctionNameCheck Nothing pure) t
					in case r of
						Just newt -> return $ Just (newt, Right (fst pure), ctx)
						Nothing -> loop xs t

mixedApplyFirstSimplificationWithPure :: (Monad m) =>
	[SimplificationF m ctx] ->
	ctx ->
	Tree ->
	m (Maybe (Tree, Either SimplifyPattern String, ctx))
mixedApplyFirstSimplificationWithPure simplifications ctx t0 =
		mixedApplyFirstSimplificationWithSimplify (makePureSimplify simplifications) simplifications ctx t0

-----------
-- LOOPS --
-----------

applySimplificationsUntil0Debug :: [SimplifyPattern] -> Tree -> [(Tree, SimplifyPattern)]
applySimplificationsUntil0Debug patterns0 t0 = loop patterns0 t0
	where
	loop patterns t = case applyFirstSimplification patterns t of
		Nothing -> []
		Just (newt, rule) -> (newt, rule) : loop patterns newt

mixedApplySimplificationsWithPureUntil0Debug :: (Monad m) =>
	[SimplificationF m ctx] ->
	ctx ->
	Tree ->
	m [(Tree, Either SimplifyPattern String, ctx)]
mixedApplySimplificationsWithPureUntil0Debug simplifications ctx0 t0 = loop simplifications ctx0 t0
	where
	pureSimplify = makePureSimplify simplifications
	loop simplifications ctx t = do
		r <- mixedApplyFirstSimplificationWithPure simplifications ctx t
		case r of
			Nothing -> return []
			Just (newt, rule, newCtx) -> do
				next <- loop simplifications newCtx newt
				return $ (newt, rule, newCtx) : next

-----------
-- UTILS --
-----------

liftPure :: (Monad m) => String -> (Tree -> Maybe Tree) -> MonadicSimplify m ctx
liftPure name pure = (name, func)
	where
	func ctx t = return $ case pure t of
		Nothing -> Nothing
		Just newt -> Just (ctx, newt)

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

-- | Using mixed rules, take pure ones and make a simplify function to use in Conditionals
makePureSimplify :: (Monad m) => [SimplificationF m ctx] -> (Tree -> Tree)
makePureSimplify simplifications = simplify
	where
	simplify :: (Tree -> Tree)
	simplify = applySimplificationsUntil0LastF firstAggregated

	firstAggregated :: (Tree -> Maybe Tree)
	firstAggregated = applyFirstSimplificationF (collectSimplify simplifications)

	applyPattern :: SimplifyPattern -> (Tree -> Maybe Tree)
	applyPattern pattern = applyTreeOne (matchAndReplace simplify pattern)

	collectSimplify :: [SimplificationF m ctx] -> [(Tree -> Maybe Tree)]
	collectSimplify [] = []
	collectSimplify (f : fs) = case f of
		Tuple30 pattern -> (applyPattern pattern) : collectSimplify fs
		Tuple31 {} -> collectSimplify fs
		Tuple32 pure -> (withFunctionNameCheck Nothing pure) : collectSimplify fs
