module Lib where

import Text.Read (readMaybe)
import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace
import Data.Char

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

data SimplifyPattern
	= SimplifyPatternRule PatternMatchPart PatternReplacePart

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

matchAndDoSomething :: PatternMatchPart -> Tree -> Maybe BindingDict
matchAndDoSomething match t = loop emptyDict match t
	where
	loop :: BindingDict -> PatternMatchPart -> Tree -> Maybe BindingDict
	loop dict match t = case match of
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
		(MatchGroup p ps) ->
			case t of
				(Branch x xs) ->
					matchGroups (p : ps) (x : xs) >>= (return . bindingConcat dict)
				(Leaf x) ->
					case ps of
						-- IMPORTANT: Singleton list could be equal to leaf! Implicit rule: ((a)) -> (a) -> a
						[] -> matchAndDoSomething p t >>= (return . bindingConcat dict)
						more -> Nothing

matchGroups :: [PatternMatchPart] -> [Tree] -> Maybe BindingDict
matchGroups ps ts = loop emptyDict ps ts
	where
	loop :: BindingDict -> [PatternMatchPart] -> [Tree] -> Maybe BindingDict
	loop dict [] [] = Just dict
	loop dict [] ts = Nothing -- Size should be equal
	loop dict ps [] = Nothing -- Size should be equal
	loop dict (p : ps) (t : ts) =
		case matchAndDoSomething p t of
			Nothing -> Nothing
			Just retDict ->
				let newDict = bindingConcat dict retDict
				in loop newDict ps ts
