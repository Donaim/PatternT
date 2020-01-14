
module PatternT.Types where

type Symbol = String

type QuoteInfo = Maybe (Char, Bool)       -- ^ Maybe (closing char, closedQ)

data Expr
	= Atom Symbol QuoteInfo
	| Group [Expr]
	deriving (Eq, Show, Read)

data Tree a
	= Leaf a
	| Branch [Tree a]

data ParseError
	= MissingOpenBracket    [Token]          -- ^ [Token] are tokens up to (not including) a bad TokenCloseBracket
	| MissingCloseBracket
	| MissingEndQuote       [Token] Token    -- ^ [Token] are tokens up to (not including) a bad TokenWord (Token)
	| ParsedEmptyBrackets   [Token]          -- ^ [Token] are tokens up to (not including) a bad ()
	deriving (Eq, Show, Read)

data PatternMatchPart a
	= Variable a
	| NameMatch a
	| VaradicMatch a
	| MatchGroup (PatternMatchPart a) [(PatternMatchPart a)]

data PatternReplacePart a
	= RVar a
	| RGroup [(PatternReplacePart a)]

data Conditional a
	= EqCond (PatternReplacePart a) (PatternReplacePart a)
	| NeqCond (PatternReplacePart a) (PatternReplacePart a)
	| ImpliesCond (PatternReplacePart a) (PatternReplacePart a)
	| LTCond (PatternReplacePart a) (PatternReplacePart a)
	| LECond (PatternReplacePart a) (PatternReplacePart a)

data SimplifyPattern a
	= SimplifyPattern (PatternMatchPart a) (PatternReplacePart a) [Conditional a]
	| TrySimplifyPattern (PatternMatchPart a) (PatternReplacePart a) [Conditional a]
	| EagerSimplifyPattern (PatternMatchPart a) (PatternReplacePart a) [Conditional a]

data ParseMatchError
	= ParseMatchErrorEmptyExprs
	| ParseMatchErrorTryGotNoBody
	| ParseMatchErrorEagerGotNoBody
	| ParseMatchErrorNoReplacePart
	| SplitFailed
	| ExpectedClosingBracket String
	| MatchEmptyTreeError
	| TokenizeError ParseError
	deriving (Eq, Show, Read)

data Either3 a b c
	= Left3 a
	| Middle3 b
	| Right3 c

data DelimiterOpts
	= DelimiterIgnoreQuotes
	| DelimiterRespectQuotes
	deriving (Eq, Show, Read)

data Token
	= TokenWord String QuoteInfo
	| TokenOpenBracket
	| TokenCloseBracket
	deriving (Eq, Show, Read)

data ParseOptions = ParseOptions
	{ fixMissingBrackets      :: Bool        -- ^ Treat "a b ) c )) d" as "(((a b) c)) d" and "(((( a b ) c ) d" as "(((( a b ) c ) d))". If false, returns Missing*Bracket
	, reportMissingEndQuote   :: Bool
	, reportEmptyBrackets     :: Bool
	} deriving (Eq, Show, Read)

data RecF t x = RecF (t (x, RecF t x))
type RecList x = RecF [] x

class (Eq a, Ord a) => PatternElement a where
	patternElemShow :: a -> String
	patternElemRead :: String -> QuoteInfo -> a

instance (Eq a) => Eq (Tree a) where
	a == b = case a of
		Leaf x -> case b of
			Leaf y -> x == y
			Branch {} -> False
		Branch xs -> case b of
			Leaf {} -> False
			Branch ys -> xs == ys

instance (Ord a) => Ord (Tree a) where
	compare a b =
		case a of
			(Leaf as) -> case b of
				(Leaf bs) ->
					compare as bs
				(Branch {}) ->
					LT -- ASSUMPTION: no singleton branches
			(Branch xs) -> case b of
				(Leaf {}) ->
					GT -- ASSUMPTION: no singleton branches
				(Branch ys) ->
					compare xs ys -- NOTE: the size of branch is the secondary thing, the most important is first element of branch

instance (Show a) => Show (Tree a) where
	show me = case me of
		Leaf l -> show l
		Branch xs -> "(" ++ concatMap ((' ' :) . show) xs ++ " )"

-- | Pairs of (function identifier, monadic action on tree that matches). The `ctx' is the patternElemRead-write context that is carried around. The monadic action also recieves aggregated simplify function
type MonadicSimplify a m ctx = (a, [Tree a -> Maybe (Tree a)] -> ctx -> Tree a -> m (Maybe (ctx, Tree a)))

-- | Pair of (function identifier, Function that accepts <aggregated simplify function> <tree to simplify> ) where aggregated simplify is a composition of all pure simplify functions that are used for applyTreeOne
type PureSimplificationF a = (a, [Tree a -> Maybe (Tree a)] -> Tree a -> Maybe (Tree a))

-- | General simplification possibilities
type SimplificationF a m ctx = Either3 (SimplifyPattern a) (MonadicSimplify a m ctx) (PureSimplificationF a)

type SimplifyTraceElem a = Either (SimplifyPattern a) String
