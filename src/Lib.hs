module Lib where

import Text.Read (readMaybe)
import Data.List
import Data.Maybe
import Debug.Trace
import Data.Char

traceS :: (Show a) => String -> a -> a
traceS text x = trace (text ++ show x) x

data Expr
	= A Atom
	| O String
	| Group [Expr]
	deriving (Eq, Show, Read)

data Atom
	= Number Double
	| Symbol String
	deriving (Eq, Show, Read)

classify :: String -> Expr
classify s =
	case readMaybe s :: Maybe Double of
		Just x ->
			A (Number x)
		Nothing ->
			if not (isAlphaNum (head s))
			then O s
			else A (Symbol s)

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
				else (classify cur : buffer, "")
			(')' : r) ->
				if null cur
				then (buffer, r)
				else (classify cur : buffer, r)

			(' ' : r) -> tokenize' newBuffer "" r
				where
				exp = classify cur
				newBuffer =
					if null cur
					then buffer
					else exp : buffer
			('(' : r) -> tokenize' newBuffer "" rest
				where
				exp = classify cur
				(inBrackets, rest) = tokenize' [] "" r
				g = Group (reverse inBrackets)
				newBuffer =
					if null cur
					then g : buffer
					else g : exp : buffer
			(c : r) ->
				tokenize' buffer (cur ++ [c]) r

-- TODO: Add support of unary operators. Implementation idea: should be of form (-x) so that there just simply no other
-- NOTE: May want to add n-ary functions. Implementation idea: should be of form (!func a b c d) so that arguments don't have operator between them
data Tree
	= Leaf Atom
	| Branch Tree Tree
	deriving (Eq, Show, Read)

-- treePriority :: Tree -> Int
-- treePriority t = case t of
-- 	Leaf {} -> 0
-- 	BranchAdd2 {} -> 1
-- 	BranchMult {} -> 2
-- 	BranchAdd1 {} -> 2

-- isOp :: Expr -> Bool
-- isOp (O op) = True
-- isOp (_)    = False

-- makeTree :: [Expr] -> Either ParseError Tree
-- makeTree exprs =
-- 	case exprs of
-- 		[] -> Left EmptyTree
-- 		[x] -> case x of
-- 			(A atom) -> Right $ Leaf atom
-- 			(Group exprs) -> makeTree exprs
-- 			(_) -> Left $ ExpectedAtom x
-- 		(x : xs) -> findmin Nothing ([], x, xs)
-- 	where
-- 	findmin :: Maybe Tree -> ([Expr], Expr, [Expr]) -> (Either ParseError Tree)
-- 	findmin current (prev, e, []) =
-- 		case current of
-- 			Just c -> Right c
-- 			Nothing -> Left $ NoOperators exprs
-- 	findmin current (prev, e, next) =
-- 		case e of
-- 			(O op) ->
-- 				case result of
-- 					Left e -> Left e
-- 					Right x -> case current of
-- 						Nothing -> findmin (Just x) follow
-- 						Just y ->
-- 							if treePriority x < treePriority y
-- 							then findmin (Just x) follow
-- 							else findmin (Just y) follow
-- 				where
-- 				result = (make1 prev op next)

-- 			(_) -> findmin current follow
-- 		where
-- 		follow = (prev ++ [e], head next, tail next)

-- data ParseError
-- 	= EArity Op [(Int, Int)] (Int, Int)
-- 	| EmptyTree
-- 	| NoOperators [Expr]
-- 	| ExpectedAtom Expr
-- 	| Other String
-- 	deriving (Eq, Show, Read)

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
