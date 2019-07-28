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

		('\"' : r) -> loop newBuffer "" rest
			where
			exp = Atom cur
			(quoted, rest) = takeQuoted r
			q = Atom quoted
			newBuffer =
				if null cur
				then q : buffer
				else q : exp : buffer

		(c : r) ->
			if isSpace c
			then loop newBuffer "" r
			else loop buffer (cur ++ [c]) r
			where
			exp = Atom cur
			newBuffer =
				if null cur
				then buffer
				else exp : buffer

takeQuoted :: String -> (String, String)
takeQuoted str = loop False [] str
	where
	loop escaped buf str = case str of
		"" -> (reverse buf, "")

		('\\' : xs) -> loop (not escaped) newbuf xs
			where newbuf = if escaped then '\\' : buf else buf

		('\"' : xs) ->
			if escaped
			then loop (not escaped) ('\"' : buf) xs
			else (reverse buf, xs)

		(x : xs) ->
			if escaped
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
parseMatch text = do
		replacePart <- maybe (Left $ SplitFailed betweenPipes) Right (maybeHead betweenPipes)
		unless (null badConds) (Left $ head badConds)

		match <- parseMatchPart beforeArrow
		replace <- parseReplacePart replacePart

		return (match, replace, goodConds)

	where
	(beforeArrow, _, afterArrow) = partitionString "->" text

	goodConds = snd partitionedBetween
	badConds = fst partitionedBetween

	partitionedBetween = partitionEithers mappedBetween
	mappedBetween = map parseCond (tail betweenPipes)
	betweenPipes = betweenPipesF afterArrow

	betweenPipesF :: String -> [String]
	betweenPipesF s = let (beforePipe, pipe, afterPipe) = partitionString "|" s
		in case pipe of
			[] -> [beforePipe]
			(_) -> beforePipe : betweenPipesF afterPipe

parseCond :: String -> Either ParseMatchError Conditional
parseCond text = swapEither $ do
	tryTwoReplacements "==" EqCond
	tryTwoReplacements "!=" NeqCond
	tryTwoReplacements "<" LTCond
	tryTwoReplacements "<=" LECond
	matchTry

	where
	tryTwoReplacements :: String -> (PatternReplacePart -> PatternReplacePart -> Conditional) -> Either Conditional ParseMatchError
	tryTwoReplacements key constructor = case partitionString key text of
		(left, [], right) -> Right $ CondExpected text
		(left, eq, right) -> swapEither $ do
			rleft <- parseReplacePart left
			rright <- parseReplacePart right
			return (constructor rleft rright)

	matchTry = case partitionString "!>" text of
		(left, [], right) -> Right $ CondExpected text
		(left, eq, right) -> swapEither $ do
			rleft <- parseReplacePart left
			rright <- parseMatchPart right
			return (NotmatchCond rleft rright)

partitionString :: String -> String -> (String, String, String)
partitionString break s =
	if breakIndex < 0
	then (s, "", "")
	else (take breakIndex s, break, drop (length break) after)

	where
	(after, breakIndex) = afterBreak break 0 s

	afterBreak :: String -> Int -> String -> (String, Int)
	afterBreak break pos [] = ("", -1)
	afterBreak break pos str =
		if isPrefixOf break str
		then (str, pos)
		else afterBreak break (pos + 1) (tail str)

parseMatchPart :: String -> Either ParseMatchError PatternMatchPart
parseMatchPart text = case tokenize text of
		Left e -> Left $ TokenizeError e
		Right ts -> exprToMatchPattern (Group ts)

exprToMatchPattern :: Expr -> Either ParseMatchError PatternMatchPart
exprToMatchPattern t = case t of
	(Atom s) ->
		case s of
			[x] -> Right $
				if isDigit x || (not (isAlpha x))
				then NameMatch s
				else Variable s
			('#' : xs) -> Right $
				BuiltinMatch $ BuiltinMatchNumber xs
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
