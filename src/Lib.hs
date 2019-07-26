module Lib where

import Text.Read (readMaybe)
import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace
import Data.Char
import Control.Monad

traceS :: (Show a) => String -> a -> a
traceS text x = trace (text ++ show x) x

type Symbol = String

data Expr
	= Atom Symbol
	| Group [Expr]
	deriving (Eq, Show, Read)

tokenize :: String -> [Expr]
tokenize s = reverse exprs
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

data Tree
	= Leaf Symbol
	| Branch [Tree]
	deriving (Eq, Show, Read)

makeTree :: [Expr] -> Either ParseError Tree
makeTree exprs = case exprs of
	[] -> Right $ Branch []
	[x] -> case x of -- NOTE: tree will not have singleton lists
		Atom sym -> Right (Leaf sym)
		Group g -> makeTree g
	xs ->
		case bad of
			[] -> Right (Branch good)
			errors -> Left (ChildrenErrors exprs errors)
		where
		childs = map (\ y -> makeTree [y]) xs
		(bad, good) = partitionEithers childs

makeTreeWithSingletons :: Expr -> Tree
makeTreeWithSingletons expr = case expr of
	Atom sym -> Leaf sym
	Group g -> Branch $ map makeTreeWithSingletons g

data ParseError
	= EmptyTree
	| FuncError [Expr] ParseError
	| ChildrenErrors [Expr] [ParseError]
	deriving (Eq, Show, Read)

data BuiltinMatchEnum
	= BuiltinMatchNumber Symbol
	deriving (Eq, Show, Read)

data PatternMatchPart
	= Variable Symbol
	| NameMatch Symbol
	| VaradicMatch Symbol
	| BuiltinMatch BuiltinMatchEnum
	| MatchGroup PatternMatchPart [PatternMatchPart]
	deriving (Eq, Show, Read)

type Number = Double

numToTree :: Number -> Tree
numToTree x = Leaf (showNoZeroes x)

treeToMaybeNum :: Tree -> Maybe Number
treeToMaybeNum t = case t of
	(Leaf s) -> case readMaybe s :: Maybe Number of
		Just x -> Just x
		Nothing -> Nothing
	(Branch {}) -> Nothing

data BuiltinRule
	= BuiltinAdd
	| BuiltinMultiply
	deriving (Eq, Show, Read)

builtinReplace :: BuiltinRule -> [PatternReplacePart] -> BindingDict -> Tree
builtinReplace rule args dict = case rule of
	BuiltinAdd -> withOp (+) 0
	BuiltinMultiply -> withOp (*) 1

	where
	rargs :: [Tree]
	rargs = map (replaceWithDict dict) args

	numCastedRargs :: [Either Tree Number]
	numCastedRargs = map numcast rargs
	numcast :: Tree -> Either Tree Number
	numcast t = case treeToMaybeNum t of
		Just x -> Right x
		Nothing -> Left t

	withOp :: (Number -> Number -> Number) -> Number -> Tree
	withOp op defaul = case withOpOnMaybeNums numCastedRargs op defaul of
		[] -> numToTree defaul
		[x] -> x
		xs -> (Branch xs)

	withOpOnMaybeNums :: [Either Tree Number] -> (Number -> Number -> Number) -> Number -> [Tree]
	withOpOnMaybeNums mnums op defaul = loop Nothing mnums
		where
		loop :: Maybe Number -> [Either Tree Number] -> [Tree]
		loop macc [] = case macc of
			Nothing -> []
			Just acc -> [numToTree acc]
		loop macc (x : xs) =
			case x of
				Right num ->
					let newacc = case macc of
						Just acc -> op acc num
						Nothing -> op defaul num
					in loop (Just newacc) xs
				Left t -> right
					where
					treeLeft = Leaf (stringifyBuiltin rule)
					treeArgs = t : withOpOnMaybeNums xs op defaul
					allArgs = case macc of
						Nothing -> treeArgs
						Just acc -> (numToTree acc) : treeArgs
					right = [Branch (treeLeft : allArgs)]


data PatternReplacePart
	= RVar Symbol
	| RBuiltin BuiltinRule [PatternReplacePart]
	| RGroup [PatternReplacePart]
	deriving (Eq, Show, Read)

data Conditional
	= EqCond PatternReplacePart PatternReplacePart
	| NeqCond PatternReplacePart PatternReplacePart
	| NotmatchCond PatternReplacePart PatternMatchPart
	deriving (Eq, Show, Read)

data SimplifyPattern
	= SimplifyPatternRule PatternMatchPart PatternReplacePart [Conditional]
	deriving (Eq, Show, Read)

type BindingDict = [(String, [Tree])]

emptyDict :: BindingDict
emptyDict = []

bindingGet :: BindingDict -> String -> Maybe [Tree]
bindingGet dict key =
	case find ((== key) . fst) dict of
		Nothing -> Nothing
		Just (k, v) -> Just v

bindingAdd :: BindingDict -> String -> [Tree] -> BindingDict
bindingAdd dict key value = (key, value) : dict

bindingConcat :: BindingDict -> BindingDict -> BindingDict
bindingConcat a b = a ++ b

checkCond :: (Tree -> Tree) -> BindingDict -> Conditional -> Bool
checkCond simplify dict cond = case cond of
	(EqCond left right) ->
		simplify (replaceWithDict dict left)
			== simplify (replaceWithDict dict right)
	(NeqCond left right) ->
		simplify (replaceWithDict dict left)
			/= simplify (replaceWithDict dict right)
	(NotmatchCond left right) ->
		isNothing $ matchWithDict dict right $ simplify (replaceWithDict dict left)

matchAndReplace :: (Tree -> Tree) -> SimplifyPattern -> Tree -> Maybe Tree
matchAndReplace simplify pattern t = case pattern of
	(SimplifyPatternRule match replace conds) ->
		case matchGetDict match t of
			Nothing -> Nothing
			Just dict ->
				if all (checkCond simplify dict) conds
				then Just (replaceWithDict dict replace)
				else Nothing

replaceWithDict :: BindingDict -> PatternReplacePart -> Tree
replaceWithDict dict replace = case replace of
	(RVar token) ->
		case bindingGet dict token of
			Just t -> case t of
				[x] -> x
				xs -> Branch xs
			Nothing -> (Leaf token)
	(RBuiltin builtin args) ->
		replaceBuiltin builtin args
	(RGroup xs) ->
		replaceRgroup xs
	where
	replaceBuiltin builtin args = builtinReplace builtin args dict

	replaceRgroup xs = Branch (loop xs)
		where
		loop [] = []
		loop (r : rs) =
			case r of
				(RVar token) ->
					case bindingGet dict token of
						Just t -> case t of
							[x] -> x : loop rs
							xs -> xs ++ loop rs -- Flattening the varargs
						Nothing -> (Leaf token) : loop rs

				(RBuiltin builtin args) -> replaceBuiltin builtin args : loop rs
				(RGroup childs) -> replaceRgroup childs : loop rs

matchGetDict :: PatternMatchPart -> Tree -> Maybe BindingDict
matchGetDict match t = matchWithDict emptyDict match t

matchWithDict :: BindingDict -> PatternMatchPart -> Tree -> Maybe BindingDict
matchWithDict dict match t =
	case match of
		(Variable bindName) ->
			matchVariable dict bindName t

		(NameMatch bindName) ->
			case t of
				(Leaf symName) ->
					if bindName == symName
					then Just (bindingAdd dict bindName [t])
					else Nothing -- Names don't match
				(Branch {}) -> -- This is not a singleton branch, so we dont ever match it
					Nothing

		(VaradicMatch bindName) ->
			matchVariable dict bindName t

		(BuiltinMatch m) ->
			matchBuiltinWithDict dict m t

		(MatchGroup p ps) ->
			case t of
				(Branch xs) ->
					matchGroups dict (p : ps) xs >>= (return . bindingConcat dict)
				(Leaf x) ->
					Nothing

matchVariable :: BindingDict -> Symbol -> Tree -> Maybe BindingDict
matchVariable dict bindName t =
	case bindingGet dict bindName of
		Just [value] ->
			if t == value
			then Just (bindingAdd dict bindName [t])
			else Nothing
		(_) ->
			Just (bindingAdd dict bindName [t])

matchBuiltinWithDict :: BindingDict -> BuiltinMatchEnum -> Tree -> Maybe BindingDict
matchBuiltinWithDict dict match t = case match of
	(BuiltinMatchNumber bindName) ->
		treeToMaybeNum t >> matchVariable dict bindName t

matchGroups :: BindingDict -> [PatternMatchPart] -> [Tree] -> Maybe BindingDict
matchGroups dict [] [] = Just dict
matchGroups dict [] ts = Nothing -- Size should be equal
matchGroups dict ps [] = Nothing -- Size should be equal
matchGroups dict (p : ps) (t : ts) = case p of
	(VaradicMatch bindName) ->
		case maybeFollowingNameMatch of
			Nothing -> Just $ bindingAdd dict bindName (t : ts) ++ followingVaradictMatches

			Just nameMatch ->
				let (varadicMatched, rest) = varadicUntilName nameMatch [] (t : ts)
				in let newDict = bindingAdd dict bindName varadicMatched
					in matchGroups newDict ps rest

	(_) -> notVaradic
	where
		notVaradic = case matchWithDict dict p t of
			Nothing -> Nothing
			Just retDict ->
				let newDict = bindingConcat dict retDict
				in matchGroups newDict ps ts

		followingVaradictMatches = loop ps -- NOTE: these did not match anything
			where
			loop [] = []
			loop (x : xs) = case x of
				(VaradicMatch bindName) -> (bindName, []) : loop xs
				(_) -> loop xs

		maybeFollowingNameMatch =
			case filter isNameMatch ps of
				((NameMatch s) : rest) -> Just s
				(_) -> Nothing

		isNameMatch x = case x of
			(NameMatch {}) -> True
			(_) -> False

		varadicUntilName :: String -> [Tree] -> [Tree] -> ([Tree], [Tree])
		varadicUntilName nameMatch buf trees =
			case trees of
				[] -> break
				(t : ts) -> case t of
					(Leaf s) ->
						if s == nameMatch
						then break
						else continue
					(_) -> continue
					where continue = varadicUntilName nameMatch (t : buf) ts
			where break = (reverse buf, trees)

data ParseMatchError
	= Unknown String
	| SplitFailed [String]
	| CondExpected String
	| ExpectedClosingBracket String
	| MatchEmptyTreeError
	| MakeTreeError ParseError
	deriving (Eq, Show, Read)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : xs) = Just x

parseMatch :: String -> Either ParseMatchError SimplifyPattern
parseMatch text = do
		replacePart <- maybe (Left $ SplitFailed betweenPipes) Right (maybeHead betweenPipes)
		unless (null badConds) (Left $ head badConds)

		match <- parseMatchPart beforeArrow
		replace <- parseReplacePart replacePart

		return (SimplifyPatternRule match replace goodConds)

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
parseCond text =
	case partitionString "!=" text of
		(left, [], right) -> secondTry
		(left, neq, right) -> do
			rleft <- parseReplacePart left
			rright <- parseReplacePart right
			return (NeqCond rleft rright)

	where
	secondTry = case partitionString "==" text of
		(left, [], right) -> thirdTry
		(left, eq, right) -> do
			rleft <- parseReplacePart left
			rright <- parseReplacePart right
			return (EqCond rleft rright)

	thirdTry = case partitionString "!>" text of
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
parseMatchPart text = exprToMatchPattern expr
	where
	tokens = tokenize text
	expr = Group tokens

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
parseReplacePart text = case tree of
		Right t -> Right (treeToReplacePattern t)
		Left e -> Left (MakeTreeError e)
	where
	tokens = tokenize text
	tree = makeTree tokens

treeToReplacePattern :: Tree -> PatternReplacePart
treeToReplacePattern t = case t of
	(Leaf s) ->
		(RVar s)

	(Branch []) ->
		(RGroup [])
	(Branch (x : xs)) -> case x of
		(Branch {}) -> group
		(Leaf s) -> case s of
			"$add" -> (RBuiltin BuiltinAdd args)
			"$mult" -> (RBuiltin BuiltinMultiply args)
			(_) -> group
		where
		group = (RGroup ((treeToReplacePattern x) : args))
		args = (map treeToReplacePattern xs)

applyTreeOne :: (Tree -> Maybe Tree) -> Tree -> Maybe Tree
applyTreeOne func t = case t of
	(Leaf s) -> func t
	(Branch childs) ->
			case loop [] childs of
				Just newme -> Just newme
				Nothing -> func t
		where
		loop :: [Tree] -> [Tree] -> Maybe Tree
		loop previus [] = Nothing
		loop previus (c : cs) = case applyTreeOne func c of
			Just newc -> Just $
				case newc of
					(Branch []) -> (Branch (previus ++ cs)) -- NOTE: erasing empty leafs!
					(Branch [x]) -> (Branch (previus ++ [x] ++ cs)) -- NOTE: erasing singletons! NOTE: the top level tree can still be a singleton, but that's ok since we will match its children anyway
					(_) -> (Branch (previus ++ [newc] ++ cs))
			Nothing -> loop (previus ++ [c]) cs

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
applyFirstSimplification :: [SimplifyPattern] -> Tree -> Maybe Tree
applyFirstSimplification patterns t0 = loop patterns t0
	where
	simplify = applySimplificationsUntil0Last patterns
	loop patterns t = case patterns of
		[] -> Nothing
		(x : xs) -> case applyTreeOne (matchAndReplace simplify x) t of
			Just newt -> Just newt
			Nothing -> loop xs t

applySimplificationsUntil0Last :: [SimplifyPattern] -> Tree -> Tree
applySimplificationsUntil0Last patterns0 t0 = loop patterns0 t0
	where
	loop patterns t = case applyFirstSimplification patterns t of
		Nothing -> t
		Just newt -> loop patterns newt

applySimplificationsUntil0 :: [SimplifyPattern] -> Tree -> [Tree]
applySimplificationsUntil0 patterns0 t0 = t0 : loop patterns0 t0
	where
	loop patterns t = case applyFirstSimplification patterns t of
		Nothing -> []
		Just newt -> newt : loop patterns newt

stringifyTree :: Tree -> String
stringifyTree t = case t of
	(Leaf s) -> s
	(Branch []) -> "()"
	(Branch (x : xs)) -> "(" ++ stringifyTree x ++ concatMap ((' ' :) . stringifyTree) xs ++ ")"

stringifyMatchPart :: PatternMatchPart -> String
stringifyMatchPart t = case t of
	(Variable s) -> s
	(NameMatch name) -> name
	(VaradicMatch name) -> name
	(BuiltinMatch m) -> stringifyBuiltinMatch m
	(MatchGroup x xs) -> "(" ++ stringifyMatchPart x ++ concatMap ((' ' :) . stringifyMatchPart) xs ++ ")"

stringifyBuiltinMatch :: BuiltinMatchEnum -> String
stringifyBuiltinMatch m = case m of
	(BuiltinMatchNumber symbol) -> '#' : symbol

stringifyReplacePart :: PatternReplacePart -> String
stringifyReplacePart t = case t of
	(RVar s) -> s
	(RBuiltin x xs) -> "(" ++ stringifyBuiltin x ++ concatMap ((' ' :) . stringifyReplacePart) xs ++ ")"
	(RGroup []) -> "()"
	(RGroup (x : xs)) -> "(" ++ stringifyReplacePart x ++ concatMap ((' ' :) . stringifyReplacePart) xs ++ ")"

stringifyCond :: Conditional -> String
stringifyCond (EqCond a b) = stringifyReplacePart a ++ " == " ++ stringifyReplacePart b
stringifyCond (NeqCond a b) = stringifyReplacePart a ++ " != " ++ stringifyReplacePart b
stringifyCond (NotmatchCond a b) = stringifyReplacePart a ++ " !> " ++ stringifyMatchPart b

stringifySimplifyPattern :: SimplifyPattern -> String
stringifySimplifyPattern p = case p of
	(SimplifyPatternRule match replace conds) -> concat $ intersperse " | " $ [stringifyMatchPart match ++ " -> " ++ stringifyReplacePart replace] ++ (map stringifyCond conds)

stringifyBuiltin :: BuiltinRule -> String
stringifyBuiltin rule = case rule of
	BuiltinAdd -> "$add"
	BuiltinMultiply -> "$mult"

-- | Strip all trailing zeroes
showNoZeroes :: (Show a) => a -> String
showNoZeroes x = if anydotq then striped else s
	where
		s = show x
		r = reverse s
		anydotq = any (== '.') s
		striped = reverse $ (dropWhile (== '.') . dropWhile (== '0')) r
