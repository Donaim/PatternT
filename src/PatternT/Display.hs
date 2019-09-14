
module PatternT.Display where

import Data.List
import PatternT.Types
import PatternT.Util

stringifyTree :: (PatternElement a) => Tree a -> String
stringifyTree t = case t of
	(Leaf s) -> patternElemShow s
	(Branch []) -> "()"
	(Branch (x : xs)) -> "(" ++ stringifyTree x ++ concatMap ((' ' :) . stringifyTree) xs ++ ")"

stringifyTree0 :: (PatternElement a) => Tree a -> String
stringifyTree0 t = case t of
	(Leaf s) -> patternElemShow s
	(Branch []) -> "()"
	(Branch (x : xs)) -> stringifyTree x ++ concatMap ((' ' :) . stringifyTree) xs

stringifyMatchPart :: (PatternElement a) => PatternMatchPart a -> String
stringifyMatchPart t = case t of
	(Variable s) -> patternElemShow s
	(NameMatch name) -> patternElemShow name
	(VaradicMatch name) -> patternElemShow name
	(MatchGroup x xs) -> "(" ++ stringifyMatchPart x ++ concatMap ((' ' :) . stringifyMatchPart) xs ++ ")"

stringifyReplacePart :: (PatternElement a) => PatternReplacePart a -> String
stringifyReplacePart t = case t of
	(RVar s) -> patternElemShow s
	(RGroup []) -> "()"
	(RGroup (x : xs)) -> "(" ++ stringifyReplacePart x ++ concatMap ((' ' :) . stringifyReplacePart) xs ++ ")"

stringifyCond :: (PatternElement a) => Conditional a -> String
stringifyCond (EqCond a b) = stringifyReplacePart a ++ " == " ++ stringifyReplacePart b
stringifyCond (NeqCond a b) = stringifyReplacePart a ++ " != " ++ stringifyReplacePart b
stringifyCond (ImpliesCond a b) = stringifyReplacePart a ++ " -> " ++ stringifyReplacePart b
stringifyCond (LTCond a b) = stringifyReplacePart a ++ " < " ++ stringifyReplacePart b
stringifyCond (LECond a b) = stringifyReplacePart a ++ " <= " ++ stringifyReplacePart b

stringifySimplifyPattern :: (PatternElement a) => SimplifyPattern a -> String
stringifySimplifyPattern pattern = case pattern of
	SimplifyPattern match replace conds -> full (basepart match replace) conds
	TrySimplifyPattern match replace conds -> full ("try (" ++ basepart match replace ++ ")") conds
	where
	full leftpart conds = concat $ intersperse " | " $ leftpart : (map stringifyCond conds)
	basepart match replace = stringifyMatchPart match ++ " -> " ++ stringifyReplacePart replace

stringifyTraceElem :: (PatternElement a) => SimplifyTraceElem a -> String
stringifyTraceElem element = case element of
	Left pattern -> stringifySimplifyPattern pattern
	Right name -> "[" ++ name ++ "]"
