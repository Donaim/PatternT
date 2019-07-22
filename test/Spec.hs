
import Lib
import Data.Either
import Data.Maybe
import System.Exit
import Control.Monad

-- strs = [ ]

comparison :: [(String, String)]
comparison =
	[ ("1 * 2",                                            "2")
	, ("1 + 2 + 3",                                        "6")
	, ("+ 1 + 2 + 3",                                      "6")
	, ("+ 1 * 2 * 3",                                      "6")
	, ("2 + 2 * 2",                                        "6")
	, ("2 * 2 + 2",                                        "6")
	, ("1 + 2 * 3",                                        "7")
	, ("1 * 2 + 3",                                        "5")
	, ("1 * 2 + + 3",                                      "5")
	, ("+1 * 2 + + 3",                                     "5")
	, ("1 + ( 2 * 3)",                                     "7")
	, ("22 + 33",                                          "55")
	, ("3x + (1x + 5x)",                                   "9x")
	, ("3x + (1x + 1 + 5x)",                               "1 + 9x")
	, ("3x + 3 + 5x + 10 + (1x + 1 + 5x)",                 "14 + 14x")
	, ("3x + 3 + 3x + 3z",                                 "3 + 6x + 3z")
	, ("3 * 3z",                                           "9z")
	, ("3 * ( 2 + 3z )",                                   "6 + 9z")
	, ("(3 + 4z) * ( 2 + 3z )",                            "6 + 17z + 12 * z * z")
	, ("bcd + ab",                                         "ab + bcd")
	, ("4 * Head + 3",                                     "3 + 4Head")
	, ("23Kappa + 3",                                      "3 + 23Kappa")
	, ("4Head * 0.5",                                      "2Head")
	, ("(2x + 1 + 2x + 3x + 5x)",                          "1 + 12x")
	, ("1 + (2x + 3 + (3x + 5))",                          "9 + 5x")
	, ("1 + (2x + 3 + 9 + 20 + (3x + 5 + (1 + 2 + 3))))",  "44 + 5x")
	, ("1 + (2 + 3 * kek  ) + 5 * (  2 + 3) + 2",          "3 + 3kek")
	, ("(a + b) * (b + c)",                                "a * b + b * b + a * c + b * c")
	]

strs    = map fst comparison
tokens  = map tokenize strs
trees   = map makeTree tokens
okTrees = snd $ partitionEithers trees
reduced = map reduce okTrees
sorted  = map sortCommutative reduced

-- remb0    = map (applyTerm reduceBrackets) sorted

optimizationStack :: [Term -> Maybe Term] -> Term -> (Term, Int)
optimizationStack fs term = loop term 0 fs
	where
	loop term counter [] = (term, counter)
	loop term counter (f : fs) =
		let (newterm, iter) = applyTerm f term
		in loop newterm (counter + iter) fs

optcycle term = optimizationStack [sortCommutative, sortAssoc, reduceAddNums, reduceAddSymbols, reduceMult, reduceDistributive] term
optloop term =
	let (newterm, iter) = optcycle term
	in if iter > 0
	   then term : optloop newterm
	   else [newterm]

optimizations = map optloop reduced
optimizationsStr = map (map stringifyTerm) optimizations

afterOptimizations = map last optimizationsStr
afterOptimizationsTest = map compareF (zip (zip (map fst comparison) afterOptimizations) (map snd comparison))
failed = catMaybes afterOptimizationsTest

compareF :: ((String, String), String) -> Maybe (String, String, String)
compareF ((original, result), test) =
	if result == test
	then Nothing
	else Just (original, result, test)

comps =
	[ (GT, ("a + b + c", "a"))
	, (GT, ("a", "1"))
	, (LT, ("a + b + c", "a * b * c"))
	, (EQ, ("a + b + c", "a + (b + c)"))
	, (LT, ("a * b", "a + b + c"))
	, (LT, ("(a + b) + d", "a + (b + c + d + e + f)"))
	, (LT, ("a + b + c", "a + b + c + d")) ]
compsResults =
	let seconds = map snd comps
	in let xs = map reduce $ snd $ partitionEithers $ map (makeTree . tokenize) $ map fst seconds
	in let ys = map reduce $ snd $ partitionEithers $ map (makeTree . tokenize) $ map snd seconds
	in zipWith compare xs ys
compsTest = map fst comps == compsResults


makeCompFancy :: (String, String, String) -> String
makeCompFancy (original, result, expected) =
	"[" ++ original ++ "] -> [" ++ result ++ "] instead of [" ++ expected ++ "]"

main :: IO ()
main = do
	unless (null failed) $ do
		putStrLn "Failed matches:"
		mapM_ (putStrLn . makeCompFancy) failed
		exitFailure
