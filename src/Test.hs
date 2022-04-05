{-# LANGUAGE TemplateHaskell #-}

module Test where

import Test.QuickCheck
import Expr
import Expr_parsing

--Check the "format" function using double quotes
prop_formatDoubleQuotes :: String -> Bool
prop_formatDoubleQuotes str = str == format formattedString
    where formattedString = "\"" ++ str ++ "\""

--Check the "format" function using single quotes
prop_formatSingleQuotes :: String -> Bool
prop_formatSingleQuotes str = str == format formattedString
    where formattedString = "'" ++ str ++ "'"

-- https://stackoverflow.com/questions/12466580/how-to-use-modifiers-with-quickcheck-positive-in-my-case
-- https://stackoverflow.com/questions/12884927/conditional-quickcheck-properties
--Check the "toInt" function
prop_toInt1 :: NonNegative Int -> Bool
prop_toInt1 (NonNegative x) = x == toInt (show x)


-- Make sure all tests are above this part
-- https://hackage.haskell.org/package/QuickCheck-2.9.2/docs/Test-QuickCheck-All.html#v:quickCheckAll
-- Performs all above tests
return []
testAll = $quickCheckAll