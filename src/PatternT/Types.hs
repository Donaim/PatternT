
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
	= MissingOpenBracket
	| MissingCloseBracket
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

type SimplifyPattern = (PatternMatchPart, PatternReplacePart, [Conditional])

data ParseMatchError
	= Unknown String
	| SplitFailed [[Expr]]
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
	| DelimiterPreserveQuotes
	deriving (Eq, Show, Read)

data TokenizeBracketsOpts
	= TokenizeReportBrackets
	| TokenizeFixBrackets
	deriving (Eq, Show, Read)

data TokenizeQuotesOpts
	= TokenizeIgnoreQuotes
	| TokenizeRespectQuotes
	deriving (Eq, Show, Read)

-- | Pairs of (function name, monadic action on tree that matches). The `ctx' is the read-write context that is carried around. The monadic action also recieves aggregated simplify function
type MonadicSimplify m ctx = (String, [Tree -> Maybe Tree] -> ctx -> Tree -> m (Maybe (ctx, Tree)))

-- | Pair of (function name, Function that accepts <aggregated simplify function> <tree to simplify> ) where aggregated simplify is a composition of all pure simplify functions that are used for applyTreeOne
type PureSimplificationF = (String, [Tree -> Maybe Tree] -> Tree -> Maybe Tree)

-- | General simplification possibilities
type SimplificationF m ctx = Either3 SimplifyPattern (MonadicSimplify m ctx) PureSimplificationF

type SimplifyTraceElem = Either SimplifyPattern String
