
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

liftPure :: (Monad m) => String -> (Tree -> Maybe Tree) -> MonadicSimplify m ctx
liftPure name pure = (name, func)
	where
	func ctx t = return $ case pure t of
		Nothing -> Nothing
		Just newt -> Just (ctx, newt)

monadicMatchAndReplace :: (Monad m) =>
	String ->
	(Tree -> m (Maybe (ctx, Tree))) ->
	Tree ->
	m (Maybe (ctx, Tree))
monadicMatchAndReplace name func tree =
	case tree of
		(Leaf s) ->
			if s == name
			then func tree
			else return Nothing
		(Branch (x : xs)) ->
			case x of
				(Leaf s) ->
					if s == name
					then func tree
					else return Nothing
				(_) -> return Nothing
		(_) -> return Nothing

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
	[EitherSimplification m ctx] ->
	ctx ->
	Tree ->
	m (Maybe (Tree, Either SimplifyPattern String, ctx))
mixedApplyFirstSimplificationWithSimplify simplify simplifications ctx t0 = loop simplifications t0
	where
	loop simplifications t = case simplifications of
		[] -> return Nothing
		(simpl : xs) -> case simpl of
				Left pattern ->
					let r = applyTreeOne (matchAndReplace simplify pattern) t
					in case r of
						Just newt -> return $ Just (newt, Left pattern, ctx)
						Nothing -> loop xs t

				Right (name, func) -> do
					r <- monadicApplyTreeOne (monadicMatchAndReplace name (func ctx)) t
					case r of
						Just (newCtx, newt) -> return $ Just (newt, Right name, newCtx)
						Nothing -> loop xs t

mixedApplyFirstSimplification :: (Monad m) =>
	[EitherSimplification m ctx] ->
	ctx ->
	Tree ->
	m (Maybe (Tree, Either SimplifyPattern String, ctx))
mixedApplyFirstSimplification simplifications ctx t0 =
		mixedApplyFirstSimplificationWithSimplify simplify simplifications ctx t0
	where
	onlyPure = fst $ partitionEithers simplifications
	simplify = applySimplificationsUntil0Last onlyPure

mixedApplyFirstSimplificationWithPure :: (Monad m) =>
	[SimplificationF m ctx] ->
	ctx ->
	Tree ->
	m (Maybe (Tree, Either SimplifyPattern String, ctx))
mixedApplyFirstSimplificationWithPure simplifications ctx t0 =
		mixedApplyFirstSimplificationWithSimplify simplify eitherSimplifications ctx t0
	where
	eitherSimplifications = map toEitherF simplifications

	-- toEitherF :: SimplificationF m ctx -> Either SimplifyPattern (MonadicSimplify m ctx) -- Not sure why this type does not work
	toEitherF simp = case simp of
		Tuple30 pattern -> Left pattern
		Tuple31 monadic -> Right monadic
		Tuple32 (name, pure) -> Right $ liftPure name pure

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
		Tuple32 (name, func) -> func : collectSimplify fs

-----------
-- LOOPS --
-----------

applySimplificationsUntil0Debug :: [SimplifyPattern] -> Tree -> [(Tree, SimplifyPattern)]
applySimplificationsUntil0Debug patterns0 t0 = loop patterns0 t0
	where
	loop patterns t = case applyFirstSimplification patterns t of
		Nothing -> []
		Just (newt, rule) -> (newt, rule) : loop patterns newt

monadicApplySimplificationsUntil0Debug :: (Monad m) =>
	[MonadicSimplify m ctx] ->
	ctx ->
	Tree ->
	m [(Tree, String, ctx)]
monadicApplySimplificationsUntil0Debug simplifications ctx0 t0 = loop simplifications ctx0 t0
	where
	loop simplifications ctx t = do
		r <- monadicApplyFirstSimplification simplifications ctx t
		case r of
			Nothing -> return []
			Just (newt, ruleName, newCtx) -> do
				next <- loop simplifications newCtx newt
				return $ (newt, ruleName, newCtx) : next

monadicApplySimplificationsUntil0Last :: (Monad m) =>
	[MonadicSimplify m ctx] ->
	ctx ->
	Tree ->
	m (Tree, ctx)
monadicApplySimplificationsUntil0Last simplifications ctx0 t0 = loop simplifications ctx0 t0
	where
	loop simplifications ctx t = do
		r <- monadicApplyFirstSimplification simplifications ctx t
		case r of
			Nothing -> return (t, ctx)
			Just (newt, ruleName, newCtx) -> do
				loop simplifications newCtx newt

mixedApplySimplificationsUntil0Last :: (Monad m) =>
	[EitherSimplification m ctx] ->
	ctx ->
	Tree ->
	m (Tree, ctx)
mixedApplySimplificationsUntil0Last simplifications ctx0 t0 = loop simplifications ctx0 t0
	where
	loop simplifications ctx t = do
		r <- mixedApplyFirstSimplification simplifications ctx t
		case r of
			Nothing -> return (t, ctx)
			Just (newt, ruleName, newCtx) -> do
				loop simplifications newCtx newt

mixedApplySimplificationsUntil0Debug :: (Monad m) =>
	[EitherSimplification m ctx] ->
	ctx ->
	Tree ->
	m [(Tree, Either SimplifyPattern String, ctx)]
mixedApplySimplificationsUntil0Debug simplifications ctx0 t0 = loop simplifications ctx0 t0
	where
	loop simplifications ctx t = do
		r <- mixedApplyFirstSimplification simplifications ctx t
		case r of
			Nothing -> return []
			Just (newt, rule, newCtx) -> do
				next <- loop simplifications newCtx newt
				return $ (newt, rule, newCtx) : next
