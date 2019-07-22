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

data PatternReplacePart
	= RVar Symbol
	| RGroup PatternReplacePart [PatternReplacePart]
	deriving (Eq, Show, Read)

data SimplifyPattern
	= SimplifyPatternRule PatternMatchPart PatternReplacePart
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
	(SimplifyPatternRule match replace) ->
		case matchGetDict match t of
			Nothing -> Nothing
			Just dict -> Just (replaceWithDict dict replace)

replaceWithDict :: BindingDict -> PatternReplacePart -> Tree
replaceWithDict dict replace = case replace of
	(RVar token) ->
		case bindingGet dict token of
			Just t -> t
			Nothing -> (Leaf token)
	(RGroup x xs) ->
		(Branch (replaceWithDict dict x) (map (replaceWithDict dict) xs))

matchGetDict :: PatternMatchPart -> Tree -> Maybe BindingDict
matchGetDict match t = matchWithDict emptyDict match t

matchWithDict :: BindingDict -> PatternMatchPart -> Tree -> Maybe BindingDict
matchWithDict dict match t = case match of
	(Variable bindName) ->
		Just (bindingAdd dict bindName t)
	(NameMatch bindName) ->
		case t of
			(Leaf symName) ->
				if bindName == symName
				then Just (bindingAdd dict bindName t)
				else Nothing -- Names don't match
			(Branch {}) ->
				Nothing -- NameMatch cannot match a tree!

	(MatchGroup p []) ->
		matchWithDict dict p t
	(MatchGroup p ps) ->
		case t of
			(Branch x []) ->
				matchWithDict dict match x
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
			return (SimplifyPatternRule match replace)

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
			('[' : x : xs) ->
				if last xs == ']'
				then NameMatch (x : (init xs))
				else Variable s
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
	(Branch x xs) ->
		(RGroup (treeToReplacePattern x) (map treeToReplacePattern xs))
