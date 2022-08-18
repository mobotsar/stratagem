module Main (main) where

import LibInternal
import Aasam
import Data.List.NonEmpty
import qualified Data.Set as Set
import qualified Data.List as List

import Test.Framework (defaultMain)
import Test.Framework.Providers.API (Test(Test))
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (Assertable(assert), Assertion, Test(..), assertEqual)

testMap :: (Eq a, Show a) => [(String, a, a)] -> [Test.HUnit.Test]
testMap = List.map (\(label, x, y) -> TestLabel label (TestCase (assertEqual "" x y)))

tests :: [Test.Framework.Providers.API.Test]
tests = hUnitTestToTests $ TestList labeledTests

main :: IO ()
main = defaultMain tests

labeledTests :: [Test.HUnit.Test]
labeledTests = testMap [("", LibInternal.lambdify (Set.singleton (Infixl 1 (fromList ["+"]))), Left (AasamError []))]
