
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

applySimplifications :: (PatternElement a) => [SimplifyPattern a] -> Tree a -> [Tree a]
applySimplifications patterns t0 = loop patterns t0
	where
	condSimplifies = makeCondSimplifies patterns
	pureSimplifies = makeTrySimplifies condSimplifies patterns
	loop patterns t = case patterns of
		[] -> []
		(x : xs) -> case applySimplifyPattern pureSimplifies condSimplifies x t of
			Just newt -> newt : (loop xs newt)
			Nothing -> loop xs t

-- Breaks on first success
applyFirstSimplification :: (PatternElement a) => [Tree a -> Maybe (Tree a)] -> [SimplifyPattern a] -> Tree a -> Maybe (Tree a, SimplifyPattern a)
applyFirstSimplification condSimplifies patterns t0 = loop patterns t0
	where
	pureSimplifies = makeTrySimplifies condSimplifies patterns
	loop patterns t = case patterns of
		[] -> Nothing
		(x : xs) -> case applySimplifyPattern pureSimplifies condSimplifies x t of
			Just newt -> Just (newt, x)
			Nothing -> loop xs t

-- Breaks on first success, does not return SimplifyPattern a that matched
applyFirstSimplificationL :: (PatternElement a) => [SimplifyPattern a] -> Tree a -> Maybe (Tree a)
applyFirstSimplificationL patterns t0 = loop patterns t0
	where
	condSimplifies = makeCondSimplifies patterns
	pureSimplifies = makeTrySimplifies condSimplifies patterns
	loop patterns t = case patterns of
		[] -> Nothing
		(x : xs) -> case applySimplifyPattern pureSimplifies condSimplifies x t of
			Just newt -> Just newt
			Nothing -> loop xs t

applyFirstSimplificationF :: (PatternElement a) => [Tree a -> Maybe (Tree a)] -> Tree a -> Maybe (Tree a)
applyFirstSimplificationF funcs t0 = loop funcs t0
	where
	loop funcs t = case funcs of
		[] -> Nothing
		(f : fs) -> case f t of
			Just newt -> Just newt
			Nothing -> loop fs t

applySimplificationsUntil0LastF :: (PatternElement a) => (Tree a -> Maybe (Tree a)) -> Tree a -> Tree a
applySimplificationsUntil0LastF func t0 = loop t0
	where
	loop t = case func t of
		Nothing -> t
		Just newt -> loop newt

applySimplificationsUntil0Last :: (PatternElement a) => [SimplifyPattern a] -> Tree a -> Tree a
applySimplificationsUntil0Last patterns t0 = loop t0
	where
	loop t = case applyFirstSimplificationL patterns t of
		Nothing -> t
		Just newt -> loop newt

-----------------------------
-- MONADIC SIMPLIFICATIONS --
-----------------------------

monadicMatchAndReplace :: (Monad m, PatternElement a) =>
	a ->
	(Tree a -> m (Maybe (ctx, Tree a))) ->
	Tree a ->
	m (Maybe (ctx, Tree a))
monadicMatchAndReplace name func =
	withFunctionNameCheck (return Nothing) (name, func)

monadicApplyFirstSimplification :: (Monad m, PatternElement a) =>
	[Tree a -> Maybe (Tree a)] ->
	[MonadicSimplify a m ctx] ->
	ctx ->
	Tree a ->
	m (Maybe (Tree a, String, ctx))
monadicApplyFirstSimplification condSimplifies simplifications ctx t0 = loop simplifications t0
	where
	loop simplifications t = case simplifications of
		[] -> return Nothing
		((name, func) : xs) -> do
			r <- monadicApplyTreeOne (monadicMatchAndReplace name (func condSimplifies ctx)) t
			case r of
				Just (newCtx, newt) -> return $ Just (newt, patternElemShow name, newCtx)
				Nothing -> loop xs t

-----------
-- MIXED --
-----------

mixedApplyFirstSimplificationWithSimplify :: (Monad m, PatternElement a) =>
	[(SimplificationF a m ctx, [Tree a -> Maybe (Tree a)])] ->
	ctx ->
	Tree a ->
	m (Maybe (Tree a, Either (SimplifyPattern a) String, ctx))
mixedApplyFirstSimplificationWithSimplify simplifications ctx t0 = loop simplifications t0
	where
	-- loop :: (PatternElement a) => [SimplificationF a m ctx] -> Tree a -> m (Maybe (Tree a, Either (SimplifyPattern a) String, ctx))
	loop cur t = case cur of
		[] -> return Nothing
		((simpl, condSimplifies) : xs) -> case simpl of
			Left3 pattern ->
				let r = applySimplifyPattern (makeTrySimplifiesFromMixed simplifications) condSimplifies pattern t -- TODO: cache (makeTrySimplifiesFromMixed simplifications)
				in case r of
					Just newt -> return $ Just (newt, Left pattern, ctx)
					Nothing -> loop xs t

			Middle3 (name, func) -> do
				r <- monadicApplyTreeOne (monadicMatchAndReplace undefined (func condSimplifies ctx)) t
				case r of
					Just (newCtx, newt) -> return $ Just (newt, Right $ patternElemShow name, newCtx)
					Nothing -> loop xs t

			Right3 (name, func) ->
				let r = applyTreeOne (matchAndReplacePureF condSimplifies (name, func)) t
				in case r of
					Just newt -> return $ Just (newt, Right $ patternElemShow name, ctx)
					Nothing -> loop xs t

-----------
-- LOOPS --
-----------

applySimplificationsUntil0Debug :: (PatternElement a) => [SimplifyPattern a] -> Tree a -> [(Tree a, SimplifyPattern a)]
applySimplificationsUntil0Debug patterns t0 = loop t0
	where
	condSimplifies = makeCondSimplifies patterns
	loop t = case applyFirstSimplification condSimplifies patterns t of
		Nothing -> []
		Just (newt, rule) -> (newt, rule) : loop newt

mixedApplySimplificationsUntil0Debug :: (Monad m, PatternElement a) =>
	Maybe Int ->
	[SimplificationF a m ctx] ->
	ctx ->
	Tree a ->
	m [(Tree a, Either (SimplifyPattern a) String, ctx)]
mixedApplySimplificationsUntil0Debug condRecLimit simplifications ctx0 t0 = loop ctx0 t0
	where
	condSimplifies = makeCondSimplifiesFromMixed condRecLimit simplifications
	loop ctx t = do
		r <- mixedApplyFirstSimplificationWithSimplify condSimplifies ctx t
		case r of
			Nothing -> return []
			Just (newt, rule, newCtx) -> do
				next <- loop newCtx newt
				return $ (newt, rule, newCtx) : next

-----------
-- UTILS --
-----------

-- | Takes a maybe-transformation and a tree to produce a list of (transformed subtree, transformed original tree, isJust flag)
-- where each element is a result of successively applied maybe-transformation
-- It goes from left to right, bottom up. Looks like scanl on lists
-- Ex: let f = someReverse, t = makeTree "(12 34) 56" in map snd (applyTryTree f t) -> [((21 34) 56), ((21 43) 56), ((43 21) 56), ((43 21) 65), (65 (43 21))"
applyTryTree :: (PatternElement a) => (Tree a -> Maybe (Tree a)) -> Tree a -> [(Tree a, Tree a, Bool)]
applyTryTree = applyLoop id
	where
	applyLoop :: (PatternElement a) => (Tree a -> Tree a) -> (Tree a -> Maybe (Tree a)) -> Tree a -> [(Tree a, Tree a, Bool)]
	applyLoop parentReconstruction f t = case t of
		Leaf s -> [new t] -- TODO: restrict to branches only for performance. So change this to "Leaf s -> []"
		Branch xs -> loop [] xs
			where
			loop left [] = [new (Branch left)]
			loop left (x : xs) = xhistory ++ loop newleft xs
				where
				xhistory = applyLoop createWholeTree f x
				newx = (\ (a, b, c) -> a) (last xhistory)   -- ASSUMPTION: `xhistory' is not empty
				newleft = left ++ [newx]                   -- TODO: this part is slow because of using `last' and appening to the end of the list
				createWholeTree newx = parentReconstruction (Branch (left ++ (newx : xs)))

		where
		new me = maybe (me, currentWholeT, False) withNewt mnewt
			where
			mnewt = f me
			currentWholeT = parentReconstruction me
			withNewt newt = (newt, parentReconstruction newt, True)

matchAndReplacePureF :: (PatternElement a) => [Tree a -> Maybe (Tree a)] -> PureSimplificationF a -> (Tree a -> Maybe (Tree a))
matchAndReplacePureF condSimplifies (name, func) = withFunctionNameCheck Nothing (name, func condSimplifies)

applySimplifyPattern :: (PatternElement a) => ([Tree a -> Maybe (Tree a)], [Tree a -> Maybe (Tree a)]) -> [Tree a -> Maybe (Tree a)] -> SimplifyPattern a -> Tree a -> Maybe (Tree a)
applySimplifyPattern (patternSimplifies, trySimplifies) condSimplifies pattern t = case pattern of
	SimplifyPattern    match replace conds -> applyTreeOne (matchAndReplace condSimplifies match replace conds) t
	EagerSimplifyPattern mtc replace conds -> applyTreeAll (matchAndReplace condSimplifies mtc   replace conds) t
	TrySimplifyPattern match replace conds -> patrec       (matchAndReplace condSimplifies match replace conds) decr t
		where
		decr = decreasingLists trySimplifies

		-- patrec :: (PatternElement a) => (Tree a -> Maybe (Tree a)) -> RecList (Tree a -> Maybe (Tree a)) -> Tree a -> Maybe (Tree a)
		patrec simpl (RecF continuations) curT = listToMaybe $ catMaybes $ concat $ continued
			where
			trees3                        = applyTryTree simpl curT
			differentTrees                = map (\ (subt, t, changedQ) -> t) $ filter (\ (subt, t, changedQ) -> changedQ) trees3
			continued                     = map treef differentTrees
			treef          t              = map ($ t) patternSimplifies ++ map (contf t) continuations -- TODO: optimize diz
			contf          t (cont, rest) = patrec cont rest t

			-- continued      :: (PatternElement a) => [[Maybe (Tree a)]]
			-- contf          :: (PatternElement a) => Tree a -> (Tree a -> Maybe (Tree a), RecList (Tree a -> Maybe (Tree a))) -> Maybe (Tree a)
			-- differentTrees :: (PatternElement a) => [Tree a]
			-- trees3         :: (PatternElement a) => [(Tree a, Tree a, Bool)]

withFunctionNameCheck :: (PatternElement a) => b -> (a, Tree a -> b) -> (Tree a -> b)
withFunctionNameCheck defaul (nameLeaf, func) tree = case tree of -- NOTE: in condSimplifies function we always check the name!
	(Leaf s) ->
		if s == nameLeaf
		then func tree
		else defaul
	(Branch (x : xs)) ->
		case x of
			(Leaf s) ->
				if s == nameLeaf
				then func tree
				else defaul
			(_) -> defaul
	(_) -> defaul

-- | Filter condSimplifies that are pure, then apply condSimplifies to them to produce literally pure functions
makeTrySimplifiesFromMixed :: (PatternElement a) => [(SimplificationF a m ctx, [Tree a -> Maybe (Tree a)])] -> ([Tree a -> Maybe (Tree a)], [Tree a -> Maybe (Tree a)])
makeTrySimplifiesFromMixed all = partitionEithers $ loop all
	where
	loop [] = []
	loop ((s, condSimplifies) : fs) = case s of
		Left3 pattern -> (makeTrySimplify condSimplifies pattern) : loop fs
		Middle3 {} -> loop fs
		Right3 s -> (Left $ matchAndReplacePureF condSimplifies s) : loop fs

-- | Take patterns and apply condSimplifies to them to produce pair of (regular patterns, try-fail patterns)
makeTrySimplifies :: (PatternElement a) => [Tree a -> Maybe (Tree a)] -> [SimplifyPattern a] -> ([Tree a -> Maybe (Tree a)], [Tree a -> Maybe (Tree a)])
makeTrySimplifies condSimplifies all = partitionEithers $ map (makeTrySimplify condSimplifies) all

makeTrySimplify :: (PatternElement a) => [Tree a -> Maybe (Tree a)] -> SimplifyPattern a -> Either (Tree a -> Maybe (Tree a)) (Tree a -> Maybe (Tree a))
makeTrySimplify condSimplifies pattern = case pattern of
	SimplifyPattern    match replace conds -> Left  $ matchAndReplace condSimplifies match replace conds
	TrySimplifyPattern match replace conds -> Right $ matchAndReplace condSimplifies match replace conds
	EagerSimplifyPattern mtc replace conds -> Left  $ matchAndReplace condSimplifies mtc   replace conds

-- | Using mixed rules, take pure ones and make condSimplifies functions for each rule from them to use in Conditionals
-- This method has 2 features:
--  * accepts optional "limit" value which makes it so that conditionals can be nested "limit" of times only
--  * returned simplifications don't contain patterns that match their own conditionals. Ex: "True -> False | True" will not result in infinite recursion caused by conditionals simplifications. NOTE: unbound recursion can still be achieved if there is cyclic Conditional dependencies, ex: "1) p0 x -> 0 | p1 a ; 2) p1 x -> 1 | p0 x", but this is preventible by limit
makeCondSimplifiesFromMixed :: (Monad m, PatternElement a) => Maybe Int -> [SimplificationF a m ctx] -> [(SimplificationF a m ctx, [Tree a -> Maybe (Tree a)])]
makeCondSimplifiesFromMixed condRecLimit simplifications = maybe (mapwithcur unlimited) (\ l -> map (\ s -> (s, limited s l 0)) simplifications) condRecLimit
	where
	-- mapwithcur :: (PatternElement a) => (SimplificationF a m ctx -> [Tree a -> Maybe (Tree a)]) -> [(SimplificationF a m ctx, [Tree a -> Maybe (Tree a)])]
	mapwithcur f = map (\ s -> (s, f s)) simplifications

	-- limited :: (PatternElement a) => SimplificationF a m ctx -> Int -> Int -> [Tree a -> Maybe (Tree a)]
	limited current limit = let f n = if n >= limit then [] else withrec current (f (n + 1)) in f

	-- unlimited :: (PatternElement a) => SimplificationF a m ctx -> [Tree a -> Maybe (Tree a)]
	unlimited current = let f = withrec current f in f

	-- withrec :: (PatternElement a) => SimplificationF a m ctx -> [Tree a -> Maybe (Tree a)] -> [Tree a -> Maybe (Tree a)]
	withrec current firstAggregated = collectSimplify simplifications
		where
		-- applyPattern :: (PatternElement a) => SimplifyPattern a -> (Tree a -> Maybe (Tree a))
		applyPattern pattern =
			if anymatched -- If any conditional gets matched by the match part of pattern, reduction usually results in infinite recursion, so skip that pattern
			then const Nothing
			else matchAndReplace firstAggregated match replace conds -- TODO: try things in conds (replace `matchAndReplace' by `applySimplifyPattern')
			where
			(match, replace, conds) = case pattern of
				SimplifyPattern match replace conds -> (match, replace, conds)
				TrySimplifyPattern match replace conds -> (match, replace, conds)
				EagerSimplifyPattern mtc replace conds -> (mtc,   replace, conds)
			condTrees = foldl (\ acc (left, s, right) -> left : right : acc) [] $ map conditionalToTrees conds
			anymatched = any isJust $ map (matchGetDict match) condTrees

		-- collectSimplify :: (PatternElement a) => [SimplificationF a m ctx] -> [(Tree a -> Maybe (Tree a))]
		collectSimplify [] = []
		collectSimplify (f : fs) = case f of
			Left3 pattern -> (applyPattern pattern) : collectSimplify fs
			Middle3 {} -> collectSimplify fs
			Right3 (name, func) -> (withFunctionNameCheck Nothing (name, func firstAggregated)) : collectSimplify fs

makeCondSimplifies :: (PatternElement a) => [SimplifyPattern a] -> [Tree a -> Maybe (Tree a)]
makeCondSimplifies patterns = let f = map (simpleApply f) patterns in f
	where
	-- TODO: try things in conds (replace `matchAndReplace' by `applySimplifyPattern')
	simpleApply condSimplifies pattern = case pattern of
		SimplifyPattern match replace conds -> matchAndReplace condSimplifies match replace conds
		TrySimplifyPattern match replace conds -> matchAndReplace condSimplifies match replace conds
		EagerSimplifyPattern mtc replace conds -> matchAndReplace condSimplifies mtc   replace conds

	-- TODO: skip patterns that match own conditionals. Based on how it's done in `makeCondSimplifiesFromMixed'
