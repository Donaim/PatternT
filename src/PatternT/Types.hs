
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
	= FreeTokensAfterClose String
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

data PatternReplacePart
	= RVar Symbol
	| RGroup [PatternReplacePart]
	deriving (Eq, Show, Read)

data Conditional
	= EqCond PatternReplacePart PatternReplacePart
	| NeqCond PatternReplacePart PatternReplacePart
	| NotmatchCond PatternReplacePart PatternMatchPart
	| LTCond PatternReplacePart PatternReplacePart
	| LECond PatternReplacePart PatternReplacePart
	deriving (Eq, Show, Read)

type SimplifyPattern = (PatternMatchPart, PatternReplacePart, [Conditional])

data ParseMatchError
	= Unknown String
	| SplitFailed [[Expr]]
	| CondExpected [Expr]
	| ExpectedClosingBracket String
	| MatchEmptyTreeError
	| TokenizeError ParseError
	deriving (Eq, Show, Read)

data Tuple3 a b c
	= Tuple30 a
	| Tuple31 b
	| Tuple32 c

-- | Pairs of (function name, monadic action on tree that matches). The `ctx' is the read-write context that is carried around
type MonadicSimplify m ctx = (String, ctx -> Tree -> m (Maybe (ctx, Tree)))

type PureSimplificationF = ((String), (Tree -> Maybe Tree))

-- | General simplification possibilities
type SimplificationF m ctx = Tuple3 SimplifyPattern (MonadicSimplify m ctx) PureSimplificationF

type SimplifyTraceElem = Either SimplifyPattern String
