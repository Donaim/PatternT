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

bindingGet :: BindingDict -> String -> Maybe Tree
bindingGet dict key =
	case find ((== key) . fst) dict of
		Nothing -> Nothing
		Just (k, v) -> Just v

bindingAdd :: BindingDict -> String -> Tree -> BindingDict
bindingAdd dict key value = (key, value) : dict

bindingConcat :: BindingDict -> BindingDict -> BindingDict
bindingConcat a b = a ++ b

matchAndDoSomething :: PatternMatchPart -> Tree -> BindingDict -> Maybe BindingDict
matchAndDoSomething match t dict = case t of
	(_) -> undefined

-- make1 :: [Expr] -> Op -> [Expr] -> Either ParseError Tree
-- make1 prev op next =
-- 	case op of
-- 		OpMult -> case prev of
-- 			[] -> arityE [(1, 1)]
-- 			xs -> case next of
-- 				[] -> arityE [(1, 1)]
-- 				ys -> do
-- 					left <- makeTree xs
-- 					right <- makeTree ys
-- 					return $ BranchMult left right

-- 		OpAdd -> case prev of
-- 			[] -> case next of
-- 				[] -> arityE [(0, 1)]
-- 				ys -> do
-- 					right <- makeTree ys
-- 					return $ BranchAdd1 right
-- 			xs -> case next of
-- 				[] -> arityE [(1, 1), (0, 1)]
-- 				ys ->
-- 					if isOp (last xs)
-- 					then
-- 						let rightGroup = Group ys
-- 						in makeTree (xs ++ [rightGroup])
-- 					else do
-- 						left <- makeTree xs
-- 						right <- makeTree ys
-- 						return $ BranchAdd2 left right
-- 	where
-- 	arityE expected = Left $ EArity op expected (length prev, length next)

-- data Term
-- 	= TNum Double
-- 	| TSym String
-- 	| TAdd Term Term
-- 	| TMul Term Term
-- 	deriving (Eq, Show, Read)
