
module PatternT.Display where

import Data.List
import PatternT.Types
import PatternT.Util

stringifyTree :: Tree -> String
stringifyTree t = case t of
	(Leaf s) -> s
	(Branch []) -> "()"
	(Branch (x : xs)) -> "(" ++ stringifyTree x ++ concatMap ((' ' :) . stringifyTree) xs ++ ")"

stringifyMatchPart :: PatternMatchPart -> String
stringifyMatchPart t = case t of
	(Variable s) -> s
	(NameMatch name) -> name
	(VaradicMatch name) -> name
	(MatchGroup x xs) -> "(" ++ stringifyMatchPart x ++ concatMap ((' ' :) . stringifyMatchPart) xs ++ ")"

stringifyReplacePart :: PatternReplacePart -> String
stringifyReplacePart t = case t of
	(RVar s) -> s
	(RGroup []) -> "()"
	(RGroup (x : xs)) -> "(" ++ stringifyReplacePart x ++ concatMap ((' ' :) . stringifyReplacePart) xs ++ ")"

stringifyCond :: Conditional -> String
stringifyCond (EqCond a b) = stringifyReplacePart a ++ " == " ++ stringifyReplacePart b
stringifyCond (NeqCond a b) = stringifyReplacePart a ++ " != " ++ stringifyReplacePart b
stringifyCond (ImpliesCond a b) = stringifyReplacePart a ++ " -> " ++ stringifyReplacePart b
stringifyCond (LTCond a b) = stringifyReplacePart a ++ " < " ++ stringifyReplacePart b
stringifyCond (LECond a b) = stringifyReplacePart a ++ " <= " ++ stringifyReplacePart b

stringifySimplifyPattern :: SimplifyPattern -> String
stringifySimplifyPattern (match, replace, conds) =
	concat $ intersperse " | " $ [stringifyMatchPart match ++ " -> " ++ stringifyReplacePart replace] ++ (map stringifyCond conds)

stringifyTraceElem :: SimplifyTraceElem -> String
stringifyTraceElem element = case element of
	Left pattern -> stringifySimplifyPattern pattern
	Right name -> "[" ++ name ++ "]"
