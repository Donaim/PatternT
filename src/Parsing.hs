module Parsing where

import Text.Read (readMaybe)
import Data.List
import Data.Maybe
import Data.Either
import Data.Char
import Control.Monad

import Types
import Util

tokenize :: String -> Either ParseError [Expr]
tokenize s = case rest of
	[] -> Right $ reverse exprs
	xs -> Left $ FreeTokensAfterClose xs
	where
	(exprs, rest) = tokenize' [] "" s

	tokenize' :: [Expr] -> String -> String -> ([Expr], String)
	tokenize' buffer cur text =
		case text of
			"" ->
				if null cur
				then (buffer, "")
				else (Atom cur : buffer, "")
			(')' : r) ->
				if null cur
				then (buffer, r)
				else (Atom cur : buffer, r)

			(' ' : r) -> tokenize' newBuffer "" r
				where
				exp = Atom cur
				newBuffer =
					if null cur
					then buffer
					else exp : buffer
			('(' : r) -> tokenize' newBuffer "" rest
				where
				exp = Atom cur
				(inBrackets, rest) = tokenize' [] "" r
				g = Group (reverse inBrackets)
				newBuffer =
					if null cur
					then g : buffer
					else g : exp : buffer
			(c : r) ->
				tokenize' buffer (cur ++ [c]) r

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

eitherAlternative :: [Either a b] -> Either a b -> Either a b
eitherAlternative [] defaul = defaul
eitherAlternative (x : xs) defaul = case x of
	Left e -> eitherAlternative xs defaul
	Right x -> Right x

parseCond :: String -> Either ParseMatchError Conditional
parseCond text = eitherAlternative
	[ eqTry
	, neqTry
	, ltTry
	, leTry
	]
	matchTry

	where
	eqTry = tryTwoReplacements "==" EqCond
	neqTry = tryTwoReplacements "!=" NeqCond
	ltTry = tryTwoReplacements "<" LTCond
	leTry = tryTwoReplacements "<=" LECond

	tryTwoReplacements :: String -> (PatternReplacePart -> PatternReplacePart -> Conditional) -> Either ParseMatchError Conditional
	tryTwoReplacements key constructor = case partitionString key text of
		(left, [], right) -> Left $ CondExpected text
		(left, eq, right) -> do
			rleft <- parseReplacePart left
			rright <- parseReplacePart right
			return (constructor rleft rright)

	matchTry = case partitionString "!>" text of
		(left, [], right) -> Left $ CondExpected text
		(left, eq, right) -> do
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
