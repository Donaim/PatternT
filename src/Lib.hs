module Lib where

import Text.Read (readMaybe)
import Data.List
import Data.Maybe
import Debug.Trace

traceS :: (Show a) => String -> a -> a
traceS text x = trace (text ++ show x) x

data Expr
	= A Atom
	| O Op
	| Group [Expr]
	deriving (Eq, Show, Read)

data Atom
	= Number Double
	| Symbol String
	deriving (Eq, Show, Read)

data Op
	= OpAdd
	| OpMult
	deriving (Eq, Show, Read)

classify :: String -> Expr
classify "*" = O OpMult
classify "+" = O OpAdd
classify s =
	case readMaybe s :: Maybe Double of
		Just x -> A $ Number x
		Nothing ->
			case maybeNum of
				Just (x, sym) -> Group [A $ Number x, O OpMult, A $ Symbol sym]
				Nothing -> A $ Symbol s
			where
			maybeNum = case catMaybes $ map (\ sub -> ((readMaybe sub) :: Maybe Double) >>= (\ x -> return (x, drop (length sub) s))) (reverse $ inits s) of
				[] -> Nothing
				(x : xs) -> Just x

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
				case classify (cur ++ [c]) of
					O op -> tokenize' ((O op) : buffer) "" r
					(_) -> tokenize' buffer (cur ++ [c]) r

data Tree
	= Leaf Atom
	| BranchMult Tree Tree
	| BranchAdd2 Tree Tree
	| BranchAdd1 Tree
	deriving (Eq, Show, Read)

treePriority :: Tree -> Int
treePriority t = case t of
	Leaf {} -> 0
	BranchAdd2 {} -> 1
	BranchMult {} -> 2
	BranchAdd1 {} -> 2

isOp :: Expr -> Bool
isOp (O op) = True
isOp (_)    = False

makeTree :: [Expr] -> Either ParseError Tree
makeTree exprs =
	case exprs of
		[] -> Left EmptyTree
		[x] -> case x of
			(A atom) -> Right $ Leaf atom
			(Group exprs) -> makeTree exprs
			(_) -> Left $ ExpectedAtom x
		(x : xs) -> findmin Nothing ([], x, xs)
	where
	findmin :: Maybe Tree -> ([Expr], Expr, [Expr]) -> (Either ParseError Tree)
	findmin current (prev, e, []) =
		case current of
			Just c -> Right c
			Nothing -> Left $ NoOperators exprs
	findmin current (prev, e, next) =
		case e of
			(O op) ->
				case result of
					Left e -> Left e
					Right x -> case current of
						Nothing -> findmin (Just x) follow
						Just y ->
							if treePriority x < treePriority y
							then findmin (Just x) follow
							else findmin (Just y) follow
				where
				result = (make1 prev op next)

			(_) -> findmin current follow
		where
		follow = (prev ++ [e], head next, tail next)

data ParseError
	= EArity Op [(Int, Int)] (Int, Int)
	| EmptyTree
	| NoOperators [Expr]
	| ExpectedAtom Expr
	| Other String
	deriving (Eq, Show, Read)

make1 :: [Expr] -> Op -> [Expr] -> Either ParseError Tree
make1 prev op next =
	case op of
		OpMult -> case prev of
			[] -> arityE [(1, 1)]
			xs -> case next of
				[] -> arityE [(1, 1)]
				ys -> do
					left <- makeTree xs
					right <- makeTree ys
					return $ BranchMult left right

		OpAdd -> case prev of
			[] -> case next of
				[] -> arityE [(0, 1)]
				ys -> do
					right <- makeTree ys
					return $ BranchAdd1 right
			xs -> case next of
				[] -> arityE [(1, 1), (0, 1)]
				ys ->
					if isOp (last xs)
					then
						let rightGroup = Group ys
						in makeTree (xs ++ [rightGroup])
					else do
						left <- makeTree xs
						right <- makeTree ys
						return $ BranchAdd2 left right
	where
	arityE expected = Left $ EArity op expected (length prev, length next)

data Term
	= TNum Double
	| TSym String
	| TAdd Term Term
	| TMul Term Term
	deriving (Eq, Show, Read)

-- | Transform syntax tree to Term tree
transform :: Tree -> Term
transform tree = case tree of
	Leaf atom ->
		case atom of
			Number x -> TNum x
			Symbol x -> TSym x
	BranchAdd2 a b ->
		case x of
			TNum nx -> case y of
				TNum ny -> TNum (nx + ny)
				(_) -> TAdd x y
			(_) -> TAdd x y
		where
		x = transform a
		y = transform b

	BranchAdd1 x -> (transform x)
	BranchMult a b ->
		case x of
			TNum nx -> case y of
				TNum ny -> TNum (nx * ny)
				(_) -> TMul x y
			(_) -> TMul x y
		where
		x = transform a
		y = transform b

maybeNext :: a -> (a -> Maybe a) -> (a -> Maybe a) -> Maybe a
maybeNext a f1 f2 =
	case f1 a of
		Nothing -> f2 a
		Just newa -> case f2 a of
			Nothing -> Just newa
			Just newest -> Just newest

reduceAddSymbols :: Term -> Maybe Term
reduceAddSymbols t = case t of

	-- x * W + y * W -> (a + b) * W
	(TAdd (TMul (TNum x) cx) (TMul (TNum y) cy)) ->
		if cx == cy
		then Just (TMul (TNum (x + y)) cx)
		else Nothing

	-- x * W + (y * W + Z) -> (x + y) * W + Z
	(TAdd (TMul (TNum x) cx) (TAdd (TMul (TNum y) cy) z)) ->
		if cx == cy
		then Just (TAdd (TMul (TNum (x + y)) cx) z)
		else Nothing

	-- W + y * W -> (1 + y) * W
	(TAdd cx (TMul (TNum y) cy)) ->
		if cx == cy
		then Just (TMul (TNum (1 + y)) cx)
		else Nothing

	(_) -> Nothing

reduceAddNums :: Term -> Maybe Term
reduceAddNums t = case t of

	-- x + y -> x + y (reduced)
	(TAdd (TNum x) (TNum y)) -> Just
		(TNum (x + y))

	-- a + (b + W) -> (a + b) + W
	(TAdd (TNum a) (TAdd (TNum b) w)) -> Just
		(TAdd (TNum (a + b)) w)

	(_) -> Nothing

reduceMult :: Term -> Maybe Term
reduceMult t = case t of

	-- x * y -> x * y (reduced)
	(TMul (TNum x) (TNum y)) -> Just
		(TNum (x * y))

	-- a * (b * W) -> (a * b) * W
	(TMul (TNum a) (TMul (TNum b) w)) -> Just
		(TMul (TNum (a * b)) w)

	(_) -> Nothing

reduceDistributive :: Term -> Maybe Term
reduceDistributive t = case t of
	-- x * (a + b) -> x * a + x * b
	(TMul x (TAdd a b)) -> Just
		(TAdd (TMul x a) (TMul x b))

	(_) -> Nothing

reduceConstants :: Term -> Maybe Term
reduceConstants t = case t of

	(TMul (TNum 0) x) -> Just
		(TNum 0)
	(TMul (TNum 1) x) -> Just
		x
	(TAdd (TNum 0) x) -> Just
		x

	(_) -> Nothing

applyTerm :: (Term -> Maybe Term) -> Term -> (Term, Int)
applyTerm func t = case t of
	TNum {} -> case func t of
		Just newt -> (newt, 1)
		Nothing -> (t, 0)
	TSym {} -> case func t of
		Just newt -> (newt, 1)
		Nothing -> (t, 0)
	TAdd a b -> case func t' of
		Just newt -> (newt, 1 + acount + bcount)
		Nothing -> (t', 0 + acount + bcount)
		where
		(newa, acount) = applyTerm func a
		(newb, bcount) = applyTerm func b
		t'             = TAdd newa newb
	TMul a b -> case func t' of
		Just newt -> (newt, 1 + acount + bcount)
		Nothing -> (t', 0 + acount + bcount)
		where
		(newa, acount) = applyTerm func a
		(newb, bcount) = applyTerm func b
		t'             = TMul newa newb

-- | Used for ordering
sizeof :: Term -> (Int, Int, Int)
sizeof t = case t of
	TNum x -> (1, 0, 0)
	TSym x -> (1, 0, 1)
	TAdd a b -> addPoints (addPoints (sizeof a) (sizeof b)) (0, 1, 0)
	TMul a b -> addPoints (addPoints (sizeof a) (sizeof b)) (0, 2, 0)

addPoints :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addPoints (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

instance Ord Term where
	compare a b = case a of
		TNum x -> case b of
			TNum y -> compare x y
			(_) -> LT
		TSym x -> case b of
			TNum {} -> GT
			TSym y -> compare x y
			(_) -> LT
		TAdd x1 y1 -> case b of
			TNum {} -> GT
			TSym {} -> GT
			TAdd x2 y2 -> compare (sizeof a) (sizeof b)
			TMul x2 y2 -> compare (sizeof a) (sizeof b)
		TMul x1 y1 -> case b of
			TNum {} -> GT
			TSym {} -> GT
			TAdd x2 y2 -> compare (sizeof a) (sizeof b)
			TMul x2 y2 -> compare (sizeof a) (sizeof b)

sortCommutative :: Term -> Maybe Term
sortCommutative t = case t of
	TNum {} -> Nothing
	TSym {} -> Nothing

	TAdd (TAdd x y) right ->
		if right < y
		then Just $ TAdd (TAdd x right) y
		else Nothing
	TAdd left (TAdd x y) ->
		if x < left
		then Just $ TAdd x (TAdd left y)
		else Nothing
	TAdd a b ->
		if a > b
		then Just $ TAdd b a
		else Nothing

	TMul (TMul x y) right ->
		if right < y
		then Just $ TMul (TMul x right) y
		else Nothing
	TMul left (TMul x y) ->
		if x < left
		then Just $ TMul x (TMul left y)
		else Nothing
	TMul a b ->
		if a > b
		then Just $ TMul b a
		else Nothing

sortAssoc :: Term -> Maybe Term
sortAssoc t = case t of
	TNum {} -> Nothing
	TSym {} -> Nothing

	TAdd (TAdd a b) y ->
		Just (TAdd a (TAdd b y))

	TMul (TMul a b) y ->
		Just (TMul a (TMul b y))

	(_) -> Nothing

-- | Strip all trailing zeroes
showNoZeroes :: (Show a) => a -> String
showNoZeroes x = if anydotq then striped else s
	where
		s = show x
		r = reverse s
		anydotq = any (== '.') s
		striped = reverse $ (dropWhile (== '.') . dropWhile (== '0')) r

-- TODO: figure out when brackets ARE required. So far it seems like they never are
stringifyTerm :: Term -> String
stringifyTerm t = case t of
	TNum x -> showNoZeroes x
	TSym x -> x
	TAdd a b -> stringifyTerm a ++ " + " ++ stringifyTerm b

	TMul a@(TNum {}) b@(TSym {}) -> stringifyTerm a ++ stringifyTerm b
	TMul a b -> stringifyTerm a ++ " * " ++ stringifyTerm b

someFunc :: IO ()
someFunc = putStrLn "someFunc"
