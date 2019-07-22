
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
	, ("1 + (2 + 3 * kek  ) + 5 * (  2 + 3) + 2",          "30 + 3kek")
	, ("(a + b) * (b + c)",                                "a * b + b * b + a * c + b * c")
	, ("1 + 0 * (3 + x)",                                  "1")
	, ("(0 + 1) * (3 + x)",                                "3 + x")
	, ("0 * x + y",                                        "y")
	, ("3y + 0 * x",                                       "3y")
	, ("x + 2x",                                           "3x")
	]

patterns :: [String]
patterns =
	[ "a [+] b -> b [+] a"
	, "x [*] (a + b) -> (x [*] a) + (x [*] b)"
	]

main :: IO ()
main = putStrLn "Test suit is not implemented yet."
