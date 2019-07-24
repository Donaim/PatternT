module Lib where

import Text.Read (readMaybe)
import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace
import Data.Char
import Data.List.Split

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
	| Branch Tree [Tree]
	deriving (Eq, Show, Read)

makeTree :: [Expr] -> Either ParseError Tree
makeTree exprs = case exprs of
	[] -> Left EmptyTree
	[x] -> case x of
		Atom sym -> Right (Leaf sym)
		Group g -> makeTree g
	(x : xs) ->
		case bad of
			[] -> case mytree of
				Left err -> Left (FuncError exprs err)
				Right me -> Right (Branch me good)
			errors -> Left (ChildrenErrors exprs errors)
		where
		mytree = makeTree [x]
		childs = map (\ y -> makeTree [y]) xs
		(bad, good) = partitionEithers childs

data ParseError
	= EmptyTree
	| FuncError [Expr] ParseError
	| ChildrenErrors [Expr] [ParseError]
	deriving (Eq, Show, Read)

data PatternMatchPart
	= Variable Symbol
	| NameMatch Symbol
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

builtinReplace :: BuiltinRule -> [PatternReplacePart] -> BindingDict -> Maybe Tree
builtinReplace rule args dict =
	if any isNothing mrargs || not (null uncastable)
	then Nothing
	else Just $ case rule of
		BuiltinAdd -> withOp (+) 0
		BuiltinMultiply -> withOp (*) 1

	where
	mrargs :: [Maybe Tree]
	mrargs = map (replaceWithDict dict) args
	rargs = map fromJust mrargs

	numCasted :: [Number]
	uncastable :: [Tree]
	(numCasted, uncastable) = loop [] rargs
		where
		loop buf [] = (reverse buf, [])
		loop buf (x : xs) = case treeToMaybeNum x of
			Nothing -> (reverse buf, (x : xs))
			Just n -> loop (n : buf) xs

	withOp :: (Number -> Number -> Number) -> Number -> Tree
	withOp op defaul = numToTree $ foldl op defaul numCasted

data PatternReplacePart
	= RVar Symbol
	| RBuiltin BuiltinRule [PatternReplacePart]
	| RGroup PatternReplacePart [PatternReplacePart]
	deriving (Eq, Show, Read)

data Conditional
	= EqCond PatternReplacePart PatternReplacePart
	| NeqCond PatternReplacePart PatternReplacePart
	deriving (Eq, Show, Read)

data SimplifyPattern
	= SimplifyPatternRule PatternMatchPart PatternReplacePart [Conditional]
	deriving (Eq, Show, Read)

type BindingDict = [(String, Tree)]

emptyDict :: BindingDict
emptyDict = []

bindingGet :: BindingDict -> String -> Maybe Tree
bindingGet dict key =
	case find ((== key) . fst) dict of
		Nothing -> Nothing
		Just (k, v) -> Just v

bindingAdd :: BindingDict -> String -> Tree -> BindingDict
bindingAdd dict key value = (key, value) : dict

bindingConcat :: BindingDict -> BindingDict -> BindingDict
bindingConcat a b = a ++ b

matchAndReplace :: SimplifyPattern -> Tree -> Maybe Tree
matchAndReplace pattern t = case pattern of
	(SimplifyPatternRule match replace conds) ->
		case matchGetDict match t of
			Nothing -> Nothing
			Just dict -> replaceWithDict dict replace

replaceWithDict :: BindingDict -> PatternReplacePart -> Maybe Tree
replaceWithDict dict replace = case replace of
	(RVar token) ->
		case bindingGet dict token of
			Just t -> Just t
			Nothing -> Just (Leaf token)
	(RBuiltin builtin args) ->
		builtinReplace builtin args dict
	(RGroup x xs) -> case replaceWithDict dict x of
		Nothing -> Nothing
		Just rx -> if any isNothing mrxs
			then Nothing
			else Just (Branch rx rxs)
		where
		mrxs = map (replaceWithDict dict) xs
		rxs = map fromJust mrxs

matchGetDict :: PatternMatchPart -> Tree -> Maybe BindingDict
matchGetDict match t = matchWithDict emptyDict match t

matchWithDict :: BindingDict -> PatternMatchPart -> Tree -> Maybe BindingDict
matchWithDict dict match t = case t of
	(Branch singletonX []) ->
		matchWithDict dict match singletonX -- Make sure that (x) = x, ((x)) = x
	(_) -> case match of
		(Variable bindName) ->
			case bindingGet dict bindName of
				Nothing ->
					Just (bindingAdd dict bindName t)
				Just value ->
					if t == value
					then Just (bindingAdd dict bindName t)
					else Nothing

		(NameMatch bindName) ->
			case t of
				(Leaf symName) ->
					if bindName == symName
					then Just (bindingAdd dict bindName t)
					else Nothing -- Names don't match
				(Branch {}) -> -- This is not a singleton branch, so we dont ever match it
					Nothing

		(MatchGroup p []) ->
			matchWithDict dict p t
		(MatchGroup p ps) ->
			case t of
				(Branch x xs) ->
					matchGroups dict (p : ps) (x : xs) >>= (return . bindingConcat dict)
				(Leaf x) ->
					Nothing

matchGroups :: BindingDict -> [PatternMatchPart] -> [Tree] -> Maybe BindingDict
matchGroups dict [] [] = Just dict
matchGroups dict [] ts = Nothing -- Size should be equal
matchGroups dict ps [] = Nothing -- Size should be equal
matchGroups dict (p : ps) (t : ts) =
	case matchWithDict dict p t of
		Nothing -> Nothing
		Just retDict ->
			let newDict = bindingConcat dict retDict
			in matchGroups newDict ps ts

data ParseMatchError
	= Unknown
	| SplitFailed [String]
	| MakeTreeError ParseError
	deriving (Eq, Show, Read)

parseMatch :: String -> Either ParseMatchError SimplifyPattern
parseMatch text = case sp of
		[matchPart, replacePart] -> do
			match <- parseMatchPart matchPart
			replace <- parseReplacePart replacePart
			return (SimplifyPatternRule match replace [])

		other -> Left (SplitFailed other)
	where
	sp = splitOn " -> " text

parseMatchPart :: String -> Either ParseMatchError PatternMatchPart
parseMatchPart text = case tree of
		Right t -> Right (treeToMatchPattern t)
		Left e -> Left (MakeTreeError e)
	where
	tokens = tokenize text
	tree = makeTree tokens

treeToMatchPattern :: Tree -> PatternMatchPart
treeToMatchPattern t = case t of
	(Leaf s) ->
		case s of
			(x : xs) ->
				if isDigit x || (not (isAlpha x))
				then NameMatch s
				else if null xs
					then Variable s
					else NameMatch s
			(_) -> Variable s
	(Branch x xs) ->
		(MatchGroup (treeToMatchPattern x) (map treeToMatchPattern xs))

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
	(Branch x xs) -> case x of
		(Branch {}) -> group
		(Leaf s) -> case s of
			"$add" -> (RBuiltin BuiltinAdd args)
			"$mult" -> (RBuiltin BuiltinMultiply args)
			(_) -> group
		where
		group = (RGroup (treeToReplacePattern x) args)
		args = (map treeToReplacePattern xs)

applyTree :: (Tree -> Maybe Tree) -> Tree -> (Tree, Int)
applyTree func t = case t of
	(Leaf s) -> case func t of
		Just newt -> (newt, 1)
		Nothing -> (t, 0)
	(Branch x xs) -> case func t' of
		Just newt -> (newt, 1 + xcount + xscount)
		Nothing -> (t', 0 + xcount + xscount)
		where
		(newx, xcount) = applyTree func x
		zipped  = map (applyTree func) xs
		newxs   = map fst zipped
		xscount = sum (map snd zipped)
		t' = Branch newx newxs

applyTreeOne :: (Tree -> Maybe Tree) -> Tree -> Maybe Tree
applyTreeOne func t = case t of
	(Leaf s) -> func t
	(Branch x xs) -> case applyTreeOne func x of
			Just newx -> Just (Branch newx xs)
			Nothing -> case loop [] xs of
				Just newme -> Just newme
				Nothing -> func t
		where
		loop :: [Tree] -> [Tree] -> Maybe Tree
		loop previus [] = Nothing
		loop previus (c : cs) = case applyTreeOne func c of
			Just newc -> Just (Branch x (previus ++ [newc] ++ cs))
			Nothing -> loop (previus ++ [c]) cs

applySimplifications :: [SimplifyPattern] -> Tree -> [Tree]
applySimplifications patterns t0 = loop patterns t0
	where
	loop patterns t = case patterns of
		[] -> []
		(x : xs) -> case applyTreeOne (matchAndReplace x) t of
			Just newt -> newt : (loop xs newt)
			Nothing -> loop xs t

-- Breaks on first success
applyFirstSimplification :: [SimplifyPattern] -> Tree -> Maybe Tree
applyFirstSimplification patterns t0 = loop patterns t0
	where
	loop patterns t = case patterns of
		[] -> Nothing
		(x : xs) -> case applyTreeOne (matchAndReplace x) t of
			Just newt -> Just newt
			Nothing -> loop xs t

applySimplificationsUntil0 :: [SimplifyPattern] -> Tree -> [Tree]
applySimplificationsUntil0 patterns0 t0 = t0 : loop patterns0 t0
	where
	loop patterns t = case applyFirstSimplification patterns t of
		Nothing -> []
		Just newt -> newt : loop patterns newt

stringifyTree :: Tree -> String
stringifyTree t = case t of
	(Leaf s) -> s
	(Branch x xs) -> "(" ++ stringifyTree x ++ concatMap ((' ' :) . stringifyTree) xs ++ ")"

stringifyMatchPart :: PatternMatchPart -> String
stringifyMatchPart t = case t of
	(Variable s) -> s
	(NameMatch name) -> name
	(MatchGroup x xs) -> "(" ++ stringifyMatchPart x ++ concatMap ((' ' :) . stringifyMatchPart) xs ++ ")"

stringifyReplacePart :: PatternReplacePart -> String
stringifyReplacePart t = case t of
	(RVar s) -> s
	(RBuiltin x xs) -> "(" ++ stringifyBuiltin x ++ concatMap ((' ' :) . stringifyReplacePart) xs ++ ")"
	(RGroup x xs) -> "(" ++ stringifyReplacePart x ++ concatMap ((' ' :) . stringifyReplacePart) xs ++ ")"

stringifyCond :: Conditional -> String
stringifyCond (EqCond a b) = stringifyReplacePart a ++ " == " ++ stringifyReplacePart b
stringifyCond (NeqCond a b) = stringifyReplacePart a ++ " != " ++ stringifyReplacePart b

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
