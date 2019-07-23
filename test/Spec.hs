
import Lib
import Data.Either
import Data.Maybe
import System.Exit
import Control.Monad

coms :: [([String], [(String, String)])]
coms = [
	(
		[ "a [+] a -> 2 * a"
		]
	,
		[ ("1 + 1",                                            "2 * 1")
		]
	),
	(
		[ "a [or] a -> a"
		, "a [and] ([not] a) -> 0"
		, "1 [or] x -> 1"
		, "1 [and] x -> x"
		, "0 [or] x -> x"
		, "0 [and] x -> 0"
		]
	,
		[ ("x or x",                                           "x")
		, ("0 and x",                                          "0")
		]
	)]

-----------
-- RULES --
-----------
srules      = map fst coms
mrules      = map (map parseMatch) srules
partitioned = map partitionEithers mrules

okRules :: [[SimplifyPattern]]
okRules    = map snd partitioned

badRules = map fst partitioned

-----------------
-- EXPRESSIONS --
-----------------
strs :: [[(String, String)]]
strs = map snd coms

exprs :: [[String]]
exprs = map (map fst) strs

corrects :: [[String]]
corrects = map (map snd) strs

treeTokens :: [[[Expr]]]
treeTokens = map (map tokenize) exprs

trees :: [[Either ParseError Tree]]
trees = map (map makeTree) treeTokens

partitionedTrees :: [([ParseError], [Tree])]
partitionedTrees = map partitionEithers trees

okTrees :: [[Tree]]
okTrees = map snd partitionedTrees

badTrees = map fst partitionedTrees

----------------
-- SIMPLIFIED --
----------------

simplified = map simpli (zip okRules okTrees)

simpli :: ([SimplifyPattern], [Tree]) -> [[Tree]]
simpli (rules, trees) =
	map (applySimplificationsUntil0 rules) trees

simpliString :: [[Tree]] -> [[String]]
simpliString trees = map (map stringifyTree) trees

simplifiedStrings :: [[[String]]]
simplifiedStrings = map simpliString simplified

simplifiedStringsLasts :: [[String]]
simplifiedStringsLasts = map (map last) simplifiedStrings

-----------
-- MATCH --
-----------

notMatches :: [Maybe (String, String, Int, Int)]
notMatches = maybify
	where
	m :: [[Maybe (String, String)]]
	m = map equalMatchQ0 (zip simplifiedStringsLasts corrects)

	r1 :: [[(Int, Maybe (String, String))]]
	r1 = map (zip [0 ..]) m
	r2 :: [(Int, [(Int, Maybe (String, String))])]
	r2 = zip [0 ..] r1
	inject :: [[(Int, Int, Maybe (String, String))]]
	inject = map (\(x, arr) -> map (\(y, m) -> (x, y, m)) arr) r2
	flat :: [(Int, Int, Maybe (String, String))]
	flat = concat inject
	maybify :: [Maybe (String, String, Int, Int)]
	maybify = map maybeF flat

	maybeF :: (Int, Int, Maybe (String, String)) -> Maybe (String, String, Int, Int)
	maybeF (x, y, m) = case m of
		Just (s1, s2) -> Just (s1, s2, x, y)
		Nothing -> Nothing

equalMatchQ0 :: ([String], [String]) -> [Maybe (String, String)]
equalMatchQ0 (results, corrects) = map equalMatchQ1 (zip results corrects)

equalMatchQ1 :: (String, String) -> Maybe (String, String)
equalMatchQ1 (s1, s2) = if s1 == s2 then Nothing else Just (s1, s2)

----------
-- MAIN --
----------
main :: IO ()
main = putStrLn "Test suit is not implemented yet."
