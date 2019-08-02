module PatternT.Parsing where

import Text.Read (readMaybe)
import Data.List
import Data.Maybe
import Data.Either
import Data.Char
import Control.Monad

import PatternT.Types
import PatternT.Util

tokenize :: String -> Either ParseError [Expr]
tokenize s = case rest of
	[] -> Right $ reverse exprs
	xs -> Left $ FreeTokensAfterClose xs
	where
	(exprs, rest) = loop [] "" s

	loop :: [Expr] -> String -> String -> ([Expr], String)
	loop buffer cur text = case text of
		"" ->
			if null cur
			then (buffer, "")
			else (Atom cur : buffer, "")
		(')' : r) ->
			if null cur
			then (buffer, r)
			else (Atom cur : buffer, r)

		('(' : r) -> loop newBuffer "" rest
			where
			exp = Atom cur
			(inBrackets, rest) = loop [] "" r
			g = Group (reverse inBrackets)
			newBuffer =
				if null cur
				then g : buffer
				else g : exp : buffer

		(c : r) ->
			if c == '\"' || c == '\''
			then loop qbuffer "" qrest
			else
				if isSpace c
				then loop spaceBuffer "" r
				else loop buffer (cur ++ [c]) r
			where
			exp = Atom cur
			spaceBuffer =
				if null cur
				then buffer
				else exp : buffer
			(quoted, qrest) = takeQuoted c r
			q = Atom quoted
			qbuffer =
				if null cur
				then q : buffer
				else q : exp : buffer

takeQuoted :: Char -> String -> (String, String)
takeQuoted qchar str = loop False [] str
	where
	loop escaped buf str = case str of
		"" -> (reverse buf, "")

		('\\' : xs) -> loop (not escaped) newbuf xs
			where newbuf = if escaped then '\\' : buf else buf

		(x : xs) ->
			if x == qchar
			then if escaped
				then loop (not escaped) (qchar : buf) xs
				else (reverse buf, xs)
			else if escaped
				then loop False (x : '\\' : buf) xs
				else loop False (x : buf) xs

makeTree :: Expr -> Tree
makeTree expr = case expr of
	Atom sym -> Leaf sym
	(Group [x]) -> makeTree x
	(Group g) -> Branch $ map makeTree g

makeTreeWithSingletons :: Expr -> Tree
makeTreeWithSingletons expr = case expr of
	Atom sym -> Leaf sym
	Group g -> Branch $ map makeTreeWithSingletons g

parseMatch :: String -> Either ParseMatchError SimplifyPattern
parseMatch text = case tokenize text of
	Left e -> Left $ TokenizeError e
	Right exprs -> withExprs exprs

	where
	withExprs exprs = do
		replacePart <- maybe (Left $ SplitFailed betweenPipes) Right (maybeHead betweenPipes)
		unless (null badConds) (Left $ head badConds)

		match <- parseMatchPart' beforeArrow
		replace <- parseReplacePart' replacePart

		return (match, replace, goodConds)

		where
		(beforeArrow, _, afterArrow) = partitionExpr "->" exprs

		goodConds = snd partitionedBetween
		badConds = fst partitionedBetween

		partitionedBetween = partitionEithers mappedBetween
		mappedBetween = map parseCond' (tail betweenPipes)
		betweenPipes = betweenPipesF afterArrow

		betweenPipesF :: [Expr] -> [[Expr]]
		betweenPipesF exprs = let (beforePipe, pipe, afterPipe) = partitionExpr "|" exprs
			in case pipe of
				Nothing -> [beforePipe]
				(_) -> beforePipe : betweenPipesF afterPipe

parseCond' :: [Expr] -> Either ParseMatchError Conditional
parseCond' exprs = swapEither $ do
	_ <- tryTwoReplacements "==" EqCond
	_ <- tryTwoReplacements "!=" NeqCond

	swapEither $ do
		rleft <- parseReplacePart' exprs
		let rright = RVar "True"
		return (EqCond rleft rright)

	where
	tryTwoReplacements :: String -> (PatternReplacePart -> PatternReplacePart -> Conditional) -> Either Conditional ParseMatchError
	tryTwoReplacements key constructor = case partitionExpr key exprs of
		(left, Nothing, right) -> Right $ SplitFailed [exprs]
		(left, Just eq, right) -> swapEither $ do
			rleft <- parseReplacePart' left
			rright <- parseReplacePart' right
			return (constructor rleft rright)

partitionExpr :: String -> [Expr] -> ([Expr], Maybe Expr, [Expr])
partitionExpr break exprs =
	case mfound of
		Nothing -> (exprs, Nothing, [])
		Just b -> (take breakIndex exprs, Just b, after)

	where
	(after, mfound, breakIndex) = afterBreak break 0 exprs

	afterBreak :: String -> Int -> [Expr] -> ([Expr], Maybe Expr, Int)
	afterBreak break pos [] = ([], Nothing, -1)
	afterBreak break pos (x : xs) = case x of
		(Atom s) ->
			if s == break
			then (xs, Just x, pos)
			else next
		(Group leafs) -> next
		where next = afterBreak break (pos + 1) xs

parseMatchPart :: String -> Either ParseMatchError PatternMatchPart
parseMatchPart text = case tokenize text of
		Left e -> Left $ TokenizeError e
		Right ts -> exprToMatchPattern (Group ts)

parseMatchPart' :: [Expr] -> Either ParseMatchError PatternMatchPart
parseMatchPart' exprs = exprToMatchPattern (Group exprs)

exprToMatchPattern :: Expr -> Either ParseMatchError PatternMatchPart
exprToMatchPattern t = case t of
	(Atom s) ->
		case s of
			[x] -> Right $
				if isDigit x || (not (isAlpha x))
				then NameMatch s
				else Variable s
			('{' : xs) ->
				if last xs == '}' -- ASSUMPTION: we know that xs is not empty because previus match would fire
				then Right $ VaradicMatch s -- NOTE: variable name is actually like "{x}", not just "x"
				else Left $ ExpectedClosingBracket s
			(_) ->
				Right $ NameMatch s
	(Group childs) ->
		case childs of
			[] -> Left MatchEmptyTreeError
			(x : xs) -> do
				unless (null badChildren) (Left $ head badChildren)
				return (unsingleton (MatchGroup (head goodChildren) (tail goodChildren)))
				where
				parsedChildren = map exprToMatchPattern childs
				(badChildren, goodChildren) = partitionEithers parsedChildren

				unsingleton child = case child of
					(MatchGroup x []) -> case x of
						(VaradicMatch {}) -> child
						(_) -> x
					(_) -> child


parseReplacePart :: String -> Either ParseMatchError PatternReplacePart
parseReplacePart text = case tokenize text of
		Left e -> Left $ TokenizeError e
		Right ts -> Right $ exprToReplacePattern (Group ts)

parseReplacePart' :: [Expr] -> Either ParseMatchError PatternReplacePart
parseReplacePart' exprs = Right $ exprToReplacePattern (Group exprs)

exprToReplacePattern :: Expr -> PatternReplacePart
exprToReplacePattern t = case t of
	(Atom s) ->
		(RVar s)

	(Group []) ->
		(RGroup [])
	(Group (x : xs)) ->
		unsingleton (RGroup ((exprToReplacePattern x) : (map exprToReplacePattern xs)))
		where
		unsingleton child = case child of
			(RGroup [x]) -> case x of
				(RVar s) ->
					if head s == '{' && last s == '}' -- NOTE: replace pattern is also aware of varadic args, but only here
					then child
					else x
				(_) -> x
			(_) -> child
