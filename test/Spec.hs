
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

----------
-- MAIN --
----------
main :: IO ()
main = putStrLn "Test suit is not implemented yet."
