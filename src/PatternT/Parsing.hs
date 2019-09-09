module PatternT.Parsing where

import Text.Read (readMaybe)
import Data.List
import Data.Maybe
import Data.Either
import Data.Char
import Control.Monad

import PatternT.Types
import PatternT.Util

tokenize :: Bool -> String -> [Token]
tokenize respectQuotes text = loop "" text
	where
	loop :: String -> String -> [Token]
	loop buffer text = case text of
		[] -> if null buffer then [] else [buffered]
		(x : xs) -> case x of
			'(' -> next TokenOpenBracket xs
			')' -> next TokenCloseBracket xs
			other ->
				if respectQuotes && x == '\'' || x == '\"'
				then next (TokenWord quoted (Just (x, qclosed))) qrest
				else if isSpace x
				then if null buffer then loop "" xs else buffered : loop "" xs
				else loop (x : buffer) xs
			where
			(quoted, qrest, qclosed) = takeQuoted x xs
		where
		buffered = TokenWord (reverse buffer) Nothing
		next tok rest =
			if null buffer
			then tok : loop "" rest
			else buffered : tok : loop "" rest

parse :: ParseOptions -> [Token] -> Either ParseError [Expr]
parse opts tokens = maybe (Right $ parseEvery tokens) Left (listToMaybe $ parseCheck opts tokens)

parseCheck :: ParseOptions -> [Token] -> [ParseError]
parseCheck opts tokens = catMaybes $ map ($ tokens) $ map snd $ filter fst $
	[ (not $ fixMissingBrackets opts, maybe Nothing (Just . maybe MissingCloseBracket (\ hist -> MissingOpenBracket hist)) . parseCheckBrackets)
	, (reportMissingEndQuote opts,    maybe Nothing (\ (hist, blame) -> Just $ MissingEndQuote hist blame) . parseCheckQuotes)
	, (reportEmptyBrackets opts,      maybe Nothing (Just . ParsedEmptyBrackets) . parseCheckEmptyBrackets)
	]

parseEvery :: [Token] -> [Expr]
parseEvery tokens = start [] tokens
	where
	start prev tokens =
		if null rest
		then cur
		else start cur rest
		where (cur, rest) = loop (if null prev then [] else [Group prev]) tokens

	loop buffer tokens = case tokens of
		[] -> if null buffer then ([], []) else (reverse buffer, [])
		(TokenOpenBracket : r) -> let (inBrackets, rest) = loop [] r in loop ((Group inBrackets) : buffer) rest
		(TokenCloseBracket : r) -> (reverse buffer, r)
		(TokenWord text qq : r) -> loop (Atom text : buffer) r

parseCheckBrackets :: [Token] -> Maybe (Maybe [Token])
parseCheckBrackets tokens = case folded of
	Left hist -> Just (Just (reverse hist))
	Right (hist, counter) -> if counter /= 0 then Just Nothing else Nothing
	where
	folded = foldl f (Right ([], 0)) tokens
	f mcounter token = case mcounter of
		Left e -> Left e
		Right (hist, counter) -> case token of
			TokenOpenBracket -> Right (newhist, counter + 1)
			TokenCloseBracket -> if counter <= 0 then Left hist else Right (newhist, counter - 1)
			other -> Right (newhist, counter)
			where newhist = token : hist

parseCheckQuotes :: [Token] -> Maybe ([Token], Token)
parseCheckQuotes tokens = loop [] tokens
	where
	loop hist [] = Nothing
	loop hist (x : xs) = case x of
		TokenWord w quoteInfo -> case quoteInfo of
			Just (c, False) -> Just (reverse hist, x)
			other -> loop (x : hist) xs
		other -> loop (x : hist) xs

parseCheckEmptyBrackets :: [Token] -> Maybe ([Token])
parseCheckEmptyBrackets tokens = loop [] tokens
	where
	loop hist tokens = case tokens of
		[] -> Nothing
		(TokenOpenBracket : TokenCloseBracket : xs) -> Just (reverse hist)
		(x : xs) -> loop (x : hist) xs

takeQuoted :: Char -> String -> (String, String, Bool)
takeQuoted qchar str = loop False [] str
	where
	loop escaped buf str = case str of
		"" -> (reverse buf, "", False)

		('\\' : xs) -> loop (not escaped) newbuf xs
			where newbuf = if escaped then '\\' : buf else buf

		(x : xs) ->
			if x == qchar
			then if escaped
				then loop (not escaped) (qchar : buf) xs
				else (reverse buf, xs, True)
			else if escaped
				then loop False (x : '\\' : buf) xs
				else loop False (x : buf) xs

-- | Usage: delimitSymbols b ["+", "**"] "1+2**3" -> "1 + 2 ** 3"
delimitSymbols :: DelimiterOpts -> [String] -> String -> String
delimitSymbols opts delimiters text =
	if opts == DelimiterIgnoreQuotes
	then foldr folder "" text
	else concat parts
	where
	parts = loop [] "" text
		where
		loop buf cur [] =
			let delimited = if null cur then [] else delimitSymbols DelimiterIgnoreQuotes delimiters (reverse cur)
			in let newBuf = if null cur then buf else (delimited : buf)
			in reverse newBuf
		loop buf cur (x : xs) =
			if x == '\'' || x == '\"'
			then let (q, next, qclosed) = takeQuoted x xs
			in let delimited = if null cur then [] else delimitSymbols DelimiterIgnoreQuotes delimiters (reverse cur)
				in let qcur = if qclosed then x : q ++ [x] else x : q
				in let newBuf = if null cur then (qcur : buf) else (qcur : delimited : buf)
				in loop newBuf "" next
			else loop buf (x : cur) xs

	sorted = reverse $ sortOn length delimiters

	folder :: Char -> String -> String
	folder c acc = case find (`isPrefixOf` joined) sorted of
		Nothing -> joined
		Just del -> ' ' : (del ++ (' ' : drop (length del) joined))
		where joined = c : acc

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
parseMatch = parseMatch' . parseEvery . tokenize True

parseMatch' :: [Expr] -> Either ParseMatchError SimplifyPattern
parseMatch' [] = Left ParseMatchErrorEmptyExprs
parseMatch' exprs =
	let (beforeArrow, split, afterArrow) = partitionExpr "->" exprs
	in if null split
		then case exprs of
			[Atom "try"] -> Left ParseMatchErrorTryGotNoBody
			(Atom "try" : x : xs) -> do
				let newxs = case x of
					Atom {} -> x : xs
					(Group atoms) -> atoms ++ xs
				let (beforeArrow, _, afterArrow) = partitionExpr "->" newxs
				(match, replace, conds) <- interparse beforeArrow afterArrow
				return $ TrySimplifyPattern match replace conds
			other -> Left SplitFailed
		else do
			(match, replace, conds) <- interparse beforeArrow afterArrow
			return $ SimplifyPattern match replace conds

	where
	interparse beforeArrow afterArrow = do
		replacePart <- maybe (Left ParseMatchErrorNoReplacePart) Right (maybeHead betweenPipes)
		unless (null badConds) (Left $ head badConds)

		match <- parseMatchPart' beforeArrow
		replace <- parseReplacePart' replacePart

		return (match, replace, goodConds)

		where
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
	_ <- tryTwoReplacements "->" ImpliesCond
	_ <- tryTwoReplacements "<" LTCond
	_ <- tryTwoReplacements "<=" LECond

	swapEither $ do
		rleft <- parseReplacePart' exprs
		let rright = RVar "True"
		return (ImpliesCond rleft rright)

	where
	tryTwoReplacements :: String -> (PatternReplacePart -> PatternReplacePart -> Conditional) -> Either Conditional ParseMatchError
	tryTwoReplacements key constructor = case partitionExpr key exprs of
		(left, Nothing, right) -> Right $ SplitFailed
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
parseMatchPart = parseMatchPart' . parseEvery . tokenize True

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
parseReplacePart = parseReplacePart' . parseEvery . tokenize True

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
