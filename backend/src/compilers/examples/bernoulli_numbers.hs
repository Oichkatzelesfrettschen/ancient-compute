{-|
Module      : BernoulliNumbers
Description : Ada Lovelace's Bernoulli Numbers Algorithm (Note G, 1843)
Copyright   : (c) ancient-compute, 2025
License     : CC-BY-4.0
Maintainer  : ancient-compute contributors

This module implements the first computer program ever published, from Ada Lovelace's
Note G in her translation of Luigi Menabrea's memoir on the Analytical Engine.

Historical Context:
  - Published: September 1843 in Taylor's Scientific Memoirs
  - Author: Augusta Ada Byron King, Countess of Lovelace
  - Significance: First algorithm intended for machine execution
  - Original: Designed for Babbage's Analytical Engine (never built)

The Bernoulli numbers B_n are defined recursively:
    B₀ = 1
    B₁ = -1/2
    Bₙ = 0 for odd n > 1
    Bₙ = -1/(n+1) × Σ(k=0 to n-1) [C(n+1, k) × Bₖ] for even n > 1

Haskell Implementation Features:
  - Type-safe rational arithmetic using Data.Ratio
  - Pure functional implementation with pattern matching
  - Lazy evaluation for efficient computation
  - Memoization through lazy infinite list
  - Type-level guarantees preventing common errors

Ancient-Compute Integration:
  This implementation demonstrates Haskell's elegance for mathematical algorithms
  while preserving Ada's original logic. Shows how modern type systems provide
  safety guarantees that mechanical computation could not.

Compilation:
  ghc -O2 -o bernoulli bernoulli_numbers.hs

Usage:
  ./bernoulli

References:
  - Lovelace, A. (1843). "Notes by the Translator"
  - Bird, R. & Wadler, P. (1988). "Introduction to Functional Programming"
-}

module BernoulliNumbers
    ( bernoulliNumber
    , bernoulliNumberMemoized
    , bernoulliSequence
    , bernoulliAsDouble
    , printBernoulliTable
    , adaLovelaceNoteGExample
    , runTests
    ) where

import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.List (genericIndex)
import Text.Printf (printf)

-- | Type alias for rational numbers (exact arithmetic)
type Rational' = Ratio Integer

-- | Calculate binomial coefficient C(n, k) = n! / (k! * (n-k)!)
--
-- Uses iterative formula to avoid factorial overflow:
-- C(n, k) = (n * (n-1) * ... * (n-k+1)) / (k * (k-1) * ... * 1)
--
-- >>> binomialCoeff 5 2
-- 10
-- >>> binomialCoeff 10 3
-- 120
binomialCoeff :: Integer -> Integer -> Integer
binomialCoeff n k
    | k < 0 || k > n = 0
    | k == 0 || k == n = 1
    | k > n - k = binomialCoeff n (n - k)  -- Optimization: C(n,k) = C(n,n-k)
    | otherwise = product [n - k + 1 .. n] `div` product [1 .. k]

-- | Calculate the nth Bernoulli number using Ada Lovelace's recursive formula.
--
-- This is the algorithm from Note G (September 1843) - the first computer program
-- ever published. Ada designed this to run on Babbage's Analytical Engine.
--
-- Mathematical Formula:
--     B₀ = 1
--     B₁ = -1/2
--     Bₙ = 0 for odd n > 1
--     Bₙ = -1/(n+1) × Σ(k=0 to n-1) [C(n+1, k) × Bₖ] for even n > 1
--
-- Complexity:
--     Time: O(n²) due to recursive summation
--     Space: O(n) for recursion depth
--
-- Examples:
--     >>> bernoulliNumber 0
--     1 % 1
--     >>> bernoulliNumber 1
--     (-1) % 2
--     >>> bernoulliNumber 2
--     1 % 6
--     >>> bernoulliNumber 8
--     (-1) % 30
--
-- Historical Note:
--     Ada's original algorithm used 25 operations on the Analytical Engine.
--     This Haskell implementation preserves the mathematical logic while using
--     pattern matching and lazy evaluation for clarity and efficiency.
bernoulliNumber :: Integer -> Rational'
bernoulliNumber n
    | n < 0     = error "Bernoulli number index must be non-negative"
    | n == 0    = 1 % 1                -- Base case: B₀ = 1
    | n == 1    = (-1) % 2             -- Base case: B₁ = -1/2
    | odd n     = 0 % 1                -- Odd Bernoulli numbers (except B₁) are zero
    | otherwise = recursiveFormula n   -- Even n > 1
  where
    -- Recursive formula: Bₙ = -1/(n+1) × Σ(k=0 to n-1) [C(n+1, k) × Bₖ]
    recursiveFormula m =
        let summation = sum [fromInteger (binomialCoeff (m + 1) k) * bernoulliNumber k
                            | k <- [0 .. m - 1]]
        in negate (summation / fromInteger (m + 1))

-- | Memoized Bernoulli numbers using lazy infinite list.
--
-- This version uses Haskell's lazy evaluation to create an infinite list
-- of Bernoulli numbers, where each element is computed only once and cached.
--
-- More efficient than the recursive version when computing multiple values.
--
-- Examples:
--     >>> bernoulliNumberMemoized 8
--     (-1) % 30
--     >>> bernoulliNumberMemoized 12
--     (-691) % 2730
bernoullisMemoized :: [Rational']
bernoullisMemoized = map bernoulliHelper [0..]
  where
    bernoulliHelper n
        | n < 0     = error "Bernoulli number index must be non-negative"
        | n == 0    = 1 % 1
        | n == 1    = (-1) % 2
        | odd n     = 0 % 1
        | otherwise = recursiveFormula n

    recursiveFormula m =
        let summation = sum [fromInteger (binomialCoeff (m + 1) k) * (bernoullisMemoized `genericIndex` k)
                            | k <- [0 .. m - 1]]
        in negate (summation / fromInteger (m + 1))

-- | Get the nth Bernoulli number from memoized list.
bernoulliNumberMemoized :: Integer -> Rational'
bernoulliNumberMemoized n
    | n < 0     = error "Bernoulli number index must be non-negative"
    | otherwise = bernoullisMemoized `genericIndex` n

-- | Generate a sequence of Bernoulli numbers from B₀ to Bₘₐₓ.
--
-- >>> bernoulliSequence 6
-- [1 % 1,(-1) % 2,1 % 6,0 % 1,(-1) % 30,0 % 1,1 % 42]
bernoulliSequence :: Integer -> [Rational']
bernoulliSequence maxN
    | maxN < 0  = error "max_n must be non-negative"
    | otherwise = take (fromIntegral maxN + 1) bernoullisMemoized

-- | Convert a Bernoulli number to Double for numerical applications.
--
-- >>> bernoulliAsDouble 2
-- 0.16666666666666666
-- >>> bernoulliAsDouble 4
-- -0.03333333333333333
bernoulliAsDouble :: Integer -> Double
bernoulliAsDouble n = fromRational (bernoulliNumber n)

-- | Print a formatted table of Bernoulli numbers.
--
-- Example output:
--     n    B_n (exact)              B_n (decimal)
--     ==================================================
--     0    1                        1.0
--     1    -1/2                     -0.5
--     2    1/6                      0.16666666666666666
printBernoulliTable :: Integer -> IO ()
printBernoulliTable maxN = do
    putStrLn "n    B_n (exact)              B_n (decimal)"
    putStrLn (replicate 70 '=')
    mapM_ printRow [0 .. maxN]
  where
    printRow n = do
        let bn = bernoulliNumberMemoized n
        when (numerator bn /= 0) $ do  -- Only print non-zero values
            let exact = show (numerator bn) ++ "/" ++ show (denominator bn)
            let exact' = if denominator bn == 1
                        then show (numerator bn)
                        else exact
            let decimal = fromRational bn :: Double
            printf "%-4d %-24s %.15f\n" n exact' decimal

    when True = const (return ())

-- | Demonstrate Ada Lovelace's Note G example.
--
-- Ada computed B₈ in her 1843 publication as the demonstration
-- of the Analytical Engine's capabilities.
adaLovelaceNoteGExample :: IO ()
adaLovelaceNoteGExample = do
    putStrLn "Ada Lovelace's Note G - Bernoulli Numbers Calculation"
    putStrLn (replicate 70 '=')
    putStrLn ""
    putStrLn "Original Context:"
    putStrLn "  Published: September 1843"
    putStrLn "  Machine: Babbage's Analytical Engine (never built)"
    putStrLn "  Variables: 9 columns (V1-V9)"
    putStrLn "  Operations: 25 steps (Load, Add, Subtract, Multiply, Divide)"
    putStrLn ""

    let n = 8
    putStrLn $ "Computing B_" ++ show n ++ " using Ada's recursive formula..."
    putStrLn ""

    let b8 = bernoulliNumberMemoized n
    putStrLn $ "Result: B_" ++ show n ++ " = " ++ formatRational b8
    putStrLn $ "Decimal: " ++ printf "%.15f" (fromRational b8 :: Double)
    putStrLn ""

    putStrLn "Verification - First 10 Bernoulli numbers:"
    printBernoulliTable 10
  where
    formatRational r
        | denominator r == 1 = show (numerator r)
        | otherwise = show (numerator r) ++ "/" ++ show (denominator r)

-- | Test suite for Bernoulli number implementation.
runTests :: IO ()
runTests = do
    putStrLn ""
    putStrLn "=== Running Bernoulli Number Tests ==="
    putStrLn ""

    let tests =
            [ ("B_0 = 1", bernoulliNumber 0 == 1 % 1)
            , ("B_1 = -1/2", bernoulliNumber 1 == (-1) % 2)
            , ("B_2 = 1/6", bernoulliNumber 2 == 1 % 6)
            , ("B_3 = 0 (odd number theorem)", bernoulliNumber 3 == 0 % 1)
            , ("B_4 = -1/30", bernoulliNumber 4 == (-1) % 30)
            , ("B_5 = 0 (odd number theorem)", bernoulliNumber 5 == 0 % 1)
            , ("B_6 = 1/42", bernoulliNumber 6 == 1 % 42)
            , ("B_8 = -1/30 (Ada's example)", bernoulliNumber 8 == (-1) % 30)
            , ("B_10 = 5/66", bernoulliNumber 10 == 5 % 66)
            , ("B_12 = -691/2730", bernoulliNumber 12 == (-691) % 2730)
            , ("B_14 = 7/6", bernoulliNumber 14 == 7 % 6)
            , ("B_20 = -174611/330", bernoulliNumber 20 == (-174611) % 330)
            , ("Memoized matches standard", bernoulliNumberMemoized 12 == bernoulliNumber 12)
            , ("Sequence length", length (bernoulliSequence 10) == 11)
            , ("All odd B_n = 0 (n=3,5,7,9)", all (\n -> bernoulliNumber n == 0 % 1) [3,5,7,9])
            ]

    let results = map runTest tests
    let passed = length (filter id results)
    let failed = length results - passed

    putStrLn ""
    putStrLn "=== Test Results ==="
    printf "Passed: %d/%d\n" passed (length results)
    printf "Failed: %d/%d\n" failed (length results)
    putStrLn ""

    if failed == 0
        then putStrLn "✓ All tests passed!"
        else putStrLn "✗ Some tests failed"
  where
    runTest (name, result) = do
        let status = if result then "✓" else "✗"
        putStrLn $ status ++ " Test: " ++ name
        result

-- | Main entry point
main :: IO ()
main = do
    -- Run Ada's historical example
    adaLovelaceNoteGExample

    -- Run test suite
    runTests
