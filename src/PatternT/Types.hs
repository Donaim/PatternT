
module PatternT.Types where

type Symbol = String

data Expr
	= Atom Symbol
	| Group [Expr]
	deriving (Eq, Show, Read)

data Tree
	= Leaf Symbol
	| Branch [Tree]
	deriving (Eq, Show, Read)

data ParseError
	= MissingOpenBracket    [Token]          -- ^ [Token] are tokens up to (not including) a bad TokenCloseBracket
	| MissingCloseBracket
	| MissingEndQuote       [Token] Token    -- ^ [Token] are tokens up to (not including) a bad TokenWord (Token)
	| ParsedEmptyBrackets   [Token]          -- ^ [Token] are tokens up to (not including) a bad ()
	deriving (Eq, Show, Read)

data PatternMatchPart
	= Variable Symbol
	| NameMatch Symbol
	| VaradicMatch Symbol
	| MatchGroup PatternMatchPart [PatternMatchPart]
	deriving (Eq, Show, Read)

data PatternReplacePart
	= RVar Symbol
	| RGroup [PatternReplacePart]
	deriving (Eq, Show, Read)

data Conditional
	= EqCond PatternReplacePart PatternReplacePart
	| NeqCond PatternReplacePart PatternReplacePart
	| ImpliesCond PatternReplacePart PatternReplacePart
	| LTCond PatternReplacePart PatternReplacePart
	| LECond PatternReplacePart PatternReplacePart
	deriving (Eq, Show, Read)

data SimplifyPattern
	= SimplifyPattern PatternMatchPart PatternReplacePart [Conditional]
	| TrySimplifyPattern PatternMatchPart PatternReplacePart [Conditional]
	deriving (Eq, Show, Read)

data ParseMatchError
	= ParseMatchErrorEmptyExprs
	| ParseMatchErrorTryGotNoBody
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
	= TokenWord String (Maybe (Char, Bool))         -- ^ Maybe (closing char, closedQ)
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

-- | Pairs of (function name, monadic action on tree that matches). The `ctx' is the read-write context that is carried around. The monadic action also recieves aggregated simplify function
type MonadicSimplify m ctx = (String, [Tree -> Maybe Tree] -> ctx -> Tree -> m (Maybe (ctx, Tree)))

-- | Pair of (function name, Function that accepts <aggregated simplify function> <tree to simplify> ) where aggregated simplify is a composition of all pure simplify functions that are used for applyTreeOne
type PureSimplificationF = (String, [Tree -> Maybe Tree] -> Tree -> Maybe Tree)

-- | Pattern that is going to be tried to be applied some number of times
type TryPattern = SimplifyPattern

-- | General simplification possibilities
type SimplificationF m ctx = Either3 SimplifyPattern (MonadicSimplify m ctx) PureSimplificationF

type SimplifyTraceElem = Either SimplifyPattern String
