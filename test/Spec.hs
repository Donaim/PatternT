
import Lib
import Data.Either
import Data.Maybe
import System.Exit
import Control.Monad

coms :: [([String], [(String, String)])]
coms = [
	(
		[ "a ((+)) a -> ((a))"
		, "a + b -> b"
		, "vanish a ->"
		]
	,
		[ ("x + (((x + x)))",                                  "x")
		, ("(((y)) ((+)) x) + y",                              "y")
		, ("(y + x) * y",                                      "(x * y)")
		, ("(z (x (vanish (a + b)))",                          "(z x)")
		]
	),
	(
		[ "a + b -> 2 * a | a == b"
		, "x * y -> $mult x y"
		, "<2 x -> true | (x * x) == x"
		, "<2 x -> false | (x * x) != x"
		, "x % #y -> y | x !> #k"
		]
	,
		[ ("1 + 1",                                            "2")
		, ("1 + 2",                                            "(1 + 2)")
		, ("2 * a",                                            "($mult 2 a)")
		, ("<2 0",                                             "true")
		, ("<2 1",                                             "true")
		, ("<2 3",                                             "false")
		, ("(1 + 1) * a",                                      "($mult 2 a)")
		, ("a % 5",                                            "5")
		, ("4 % 5",                                            "(4 % 5)")
		]
	),
	(
		[ "a or a -> a"
		, "a and a -> a"
		, "a or (not a) -> 1"
		, "a and (not a) -> 0"
		, "1 or x -> 1"
		, "1 and x -> x"
		, "0 or x -> x"
		, "0 and x -> 0"
		, "not (not a) -> a"
		, "not 0 -> 1"
		, "not 1 -> 0"
		, "({ys} or {zs}) or x -> x or ({ys} or {zs})"
		]
	,
		[ ("x or x",                                           "x")
		, ("0 and x",                                          "0")
		, ("(0 and x) or 1",                                   "1")
		, ("0 and (x or x)",                                   "0")
		, ("(0 and x) and x",                                  "0")
		, ("not ((0 and x) and x)",                            "1")
		, ("(not ((0 and x) and x)) and x",                    "x")
		, ("(a or b) or c",                                    "(c or (a or b))")
		, ("(a z or b) or c",                                  "(c or (a z or b))")
		, ("(or b) or c",                                      "(c or (or b))")
		]
	),
	(
		[ "id a -> a"
		, "true a b -> a"
		, "false a b -> b "

		, "+ 0 b -> b"
		, "+ (succ k) b -> succ (+ k b)"

		, "* 0 b -> 0"
		, "* (succ a) b -> + b (* a b)"

		, "> 0 b -> false"
		, "> a 0 -> true"
		, "> (succ a) (succ b) -> > a b"

		, "fac 0 -> (succ 0)"
		, "fac (succ x) -> * (succ x) (fac x)"

		, "show 0 -> 0"
		, "show (succ x) -> $add 1 (show x)"

		-- , "(f x) y -> f x y" -- CARRY
		, "$add #a #b -> $add a b" -- FIX?

		, "inf -> succ inf" -- careful
		]
	,
		[ ("id x",                                           "x")
		, ("true (false a y) a",                             "y")
		, ("+ (succ (succ 0)) (succ 0)",                     "(succ (succ (succ 0)))")
		, ("show (+ (succ (succ 0)) (succ 0))",              "3")
		, ("show (fac (succ (succ (succ (succ 0",            "24")
		-- , ("show (fac (succ (succ (succ (succ (succ 0",      "120")
		-- , ("show (fac (succ (succ (succ (succ (succ (succ 0","720") -- ACTUALLY HALTS XD
		, ("true skipped (show (fac (succ (succ (succ (succ (succ 0", "skipped") -- LAZY
		, ("> (succ 0) (succ (succ 0))",                     "false")
		, ("> inf (succ (succ 0))",                          "true")
		, ("> (succ (succ 0)) inf",                          "false")
		]
	),
	(
		[ "#a + #b -> $add a b"
		, "#a * #b -> $mult a b"

		, "w + #a -> a + w                | w !> #k"
		, "w * #a -> a * w                | w !> #k"
		, "#a + (#b + w) -> (a + b) + w"              -- associative+
		, "#a * (#b * w) -> (a * b) * w"              -- associative*
		, "#c * (x + y) -> (c * x) + (c * y)"         -- distributive
		]
	,
		[ ("1 + (2 + 3)",                                      "6")
		, ("(((1 + (2 + (3 * kek)  )) + 5) * (  2 + 3)) + 2",  "(42 + (15 * kek))")
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

okRulesStr :: [[String]]
okRulesStr = map (map stringifySimplifyPattern) okRules

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

okTreesStr :: [[String]]
okTreesStr = map (map stringifyTree) okTrees

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

-------------
-- DISPLAY --
-------------

displays :: [String]
displays = map display (catMaybes notMatches)
	where
	display :: (String, String, Int, Int) -> String
	display (result, correct, x, y) = "\
		\With this set of rules:\n" ++ rules ++ "\
		\The result of:\n\t" ++ original ++ "\n\
		\Was:\n\t" ++ result ++ "\n\
		\Expected:\n\t" ++ correct ++ "\n\
		\The reductions were:\n" ++ reductions ++ "\n"
		where
		rules = unlines (map ("\t" ++) (okRulesStr !! x))
		original = ((okTreesStr !! x) !! y)
		reductions = unlines (map ("\t" ++) ((simplifiedStrings !! x) !! y))

----------
-- MAIN --
----------
main :: IO ()
main = do
	unless (all null badTrees) $ do
		putStrLn $ "Bad trees:" ++ show badTrees
		exitFailure
	unless (all null badRules) $ do
		putStrLn $ "Bad rules: " ++ show badRules
		exitFailure
	unless (null displays) $ do
		mapM_ putStrLn displays
		exitFailure
