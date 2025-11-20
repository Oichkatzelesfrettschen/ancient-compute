{-|
Module      : EgyptianMultiplication
Description : Ancient Egyptian Multiplication Algorithm (c. 2000 BCE)
Copyright   : Ancient Compute Project, 2025
License     : MIT
Maintainer  : ancient-compute@example.com
Stability   : Educational
Portability : Portable

Historical Context:
The Egyptian multiplication algorithm, documented in the Rhind Mathematical
Papyrus (c. 1650 BCE) and Moscow Mathematical Papyrus (c. 1890 BCE), represents
one of humanity's earliest algorithmic procedures. This method, using only
doubling and addition, prefigures binary arithmetic by over 3,000 years.

The algorithm demonstrates several key insights:
1. Multiplication can be decomposed into powers of 2
2. Any computation can be reduced to simpler operations
3. Recursive structure emerges naturally from the problem

In Haskell, we can express this algorithm with particular elegance, showing
how ancient mathematical insights align with modern functional programming.

Time Complexity: O(log n) where n is the smaller multiplicand
Space Complexity: O(log n) for the recursion stack
-}

module EgyptianMultiplication where

import Data.Bits ((.&.), shiftL, shiftR)
import Data.List (unfoldr, foldl')
import Control.Monad (when)
import System.CPUTime
import Text.Printf

-- | A step in the Egyptian multiplication process
data MultiplicationStep = MultiplicationStep
    { stepLeft     :: Integer  -- ^ Left column value
    , stepRight    :: Integer  -- ^ Right column value
    , stepIncluded :: Bool     -- ^ Whether this step contributes to result
    , stepSum      :: Integer  -- ^ Running sum
    } deriving (Show, Eq)

-- | Basic Egyptian multiplication for positive integers
-- This is the pure functional implementation
egyptianMultiply :: Integer -> Integer -> Integer
egyptianMultiply a b = go a b 0
  where
    go _ 0 acc = acc
    go left right acc
        | right .&. 1 == 1 = go (left `shiftL` 1) (right `shiftR` 1) (acc + left)
        | otherwise        = go (left `shiftL` 1) (right `shiftR` 1) acc

-- | Recursive implementation showing the algorithm's natural recursion
egyptianMultiplyRec :: Integer -> Integer -> Integer
egyptianMultiplyRec _ 0 = 0
egyptianMultiplyRec a 1 = a
egyptianMultiplyRec a b
    | odd b     = a + egyptianMultiplyRec (a * 2) (b `div` 2)
    | otherwise = egyptianMultiplyRec (a * 2) (b `div` 2)

-- | Point-free style implementation using unfold
-- This shows how the algorithm is essentially an unfold operation
egyptianMultiplyPointFree :: Integer -> Integer -> Integer
egyptianMultiplyPointFree a b =
    sum . map fst . filter (odd . snd) $ unfoldr step (a, b)
  where
    step (_, 0) = Nothing
    step (x, y) = Just ((x, y), (x `shiftL` 1, y `shiftR` 1))

-- | Extended version handling negative numbers
-- Ancient Egyptians didn't have negatives, but we extend the algorithm
egyptianMultiplyExtended :: Integer -> Integer -> Integer
egyptianMultiplyExtended a b = sign * egyptianMultiply (abs a) (abs b)
  where
    sign = if (a < 0) /= (b < 0) then -1 else 1

-- | Verbose version that collects all calculation steps
egyptianMultiplyVerbose :: Integer -> Integer -> (Integer, [MultiplicationStep])
egyptianMultiplyVerbose a b = go a b 0 []
  where
    go _ 0 acc steps = (acc, reverse steps)
    go left right acc steps =
        let included = odd right
            newAcc = if included then acc + left else acc
            step = MultiplicationStep left right included newAcc
        in go (left `shiftL` 1) (right `shiftR` 1) newAcc (step : steps)

-- | Display the calculation process in table format
displayCalculation :: Integer -> Integer -> IO ()
displayCalculation a b = do
    putStrLn $ "\nEgyptian Multiplication of " ++ show a ++ " × " ++ show b ++ ":"
    putStrLn $ printf "%-15s %-15s %-10s %s" "Left" "Right" "Include?" "Sum"
    putStrLn $ replicate 55 '-'

    let (result, steps) = egyptianMultiplyVerbose a b
    mapM_ printStep steps

    putStrLn $ "\nResult: " ++ show result
    putStrLn $ "Verification: " ++ show a ++ " × " ++ show b ++ " = " ++
               show (a * b) ++ " (modern)"
  where
    printStep (MultiplicationStep l r inc s) =
        putStrLn $ printf "%-15d %-15d %-10s %s"
            l r (if inc then "Yes" else "No") (if inc then show s else "")

-- | Alternative implementation using list comprehension
-- This shows the algorithm's connection to summing powers of 2
egyptianMultiplyListComp :: Integer -> Integer -> Integer
egyptianMultiplyListComp a b =
    sum [left | (left, right) <- takeWhile ((> 0) . snd) $
                iterate (\(l, r) -> (l `shiftL` 1, r `shiftR` 1)) (a, b),
                odd right]

-- | Monadic version for educational purposes
egyptianMultiplyMonadic :: Integer -> Integer -> IO Integer
egyptianMultiplyMonadic a b = do
    putStrLn "Starting Egyptian multiplication..."
    result <- go a b 0
    putStrLn $ "Final result: " ++ show result
    return result
  where
    go _ 0 acc = return acc
    go left right acc = do
        when (odd right) $
            putStrLn $ "Adding " ++ show left ++ " to accumulator"
        go (left `shiftL` 1) (right `shiftR` 1) $
            if odd right then acc + left else acc

-- | Analyze the binary connection
analyzeBinaryConnection :: Integer -> IO ()
analyzeBinaryConnection n = do
    putStrLn $ "\n=== Binary Analysis of " ++ show n ++ " ==="
    putStrLn $ "Decimal: " ++ show n
    putStrLn $ "Binary:  " ++ showBinary n
    putStrLn $ "Powers of 2: " ++ showPowersOf2 n
    putStrLn "This demonstrates why Egyptian multiplication works:"
    putStrLn "It decomposes numbers into powers of 2!"
  where
    showBinary 0 = "0"
    showBinary x = reverse $ go x
      where go 0 = ""
            go y = (if odd y then '1' else '0') : go (y `div` 2)

    showPowersOf2 x = unwords [show (2^i) | i <- [0..], let bit = (x `shiftR` i) .&. 1, bit == 1]

-- | Benchmark different implementations
benchmarkImplementations :: IO ()
benchmarkImplementations = do
    putStrLn "\n=== Performance Comparison ==="
    putStrLn "Testing different implementations (1000000 iterations each):\n"

    let testCases = [(13, 17), (127, 42), (999, 888), (12345, 6789)]

    mapM_ testCase testCases
  where
    testCase (a, b) = do
        putStrLn $ show a ++ " × " ++ show b ++ ":"

        -- Test basic implementation
        t1 <- timeIt $ replicateM_ 1000000 $ return $! egyptianMultiply a b
        putStrLn $ "  Basic:      " ++ show t1 ++ " seconds"

        -- Test recursive implementation
        t2 <- timeIt $ replicateM_ 1000000 $ return $! egyptianMultiplyRec a b
        putStrLn $ "  Recursive:  " ++ show t2 ++ " seconds"

        -- Test point-free implementation
        t3 <- timeIt $ replicateM_ 1000000 $ return $! egyptianMultiplyPointFree a b
        putStrLn $ "  Point-free: " ++ show t3 ++ " seconds"

        -- Test native multiplication
        t4 <- timeIt $ replicateM_ 1000000 $ return $! (a * b)
        putStrLn $ "  Native:     " ++ show t4 ++ " seconds\n"

    timeIt action = do
        start <- getCPUTime
        action
        end <- getCPUTime
        return $ fromIntegral (end - start) / 10^12

    replicateM_ 0 _ = return ()
    replicateM_ n action = action >> replicateM_ (n-1) action

-- | Historical examples from ancient papyri
historicalExamples :: IO ()
historicalExamples = do
    putStrLn "\n=== Historical Examples ==="

    putStrLn "\nExample from Rhind Papyrus (Problem 79):"
    putStrLn "Seven houses contain seven cats..."
    putStrLn "This involves powers of 7:"
    displayCalculation 7 2401  -- 7^4

    putStrLn "\nExample showing doubling sequence:"
    displayCalculation 1 64    -- Pure doubling to 2^6

-- | Properties that can be verified with QuickCheck
-- These demonstrate the algorithm's correctness

-- Property: Egyptian multiplication equals standard multiplication
prop_correctness :: Integer -> Integer -> Bool
prop_correctness a b = egyptianMultiply (abs a) (abs b) == (abs a) * (abs b)

-- Property: Commutative
prop_commutative :: Integer -> Integer -> Bool
prop_commutative a b = egyptianMultiply (abs a) (abs b) == egyptianMultiply (abs b) (abs a)

-- Property: Identity
prop_identity :: Integer -> Bool
prop_identity a = egyptianMultiply (abs a) 1 == abs a

-- Property: Zero
prop_zero :: Integer -> Bool
prop_zero a = egyptianMultiply (abs a) 0 == 0

-- | Main demonstration
main :: IO ()
main = do
    putStrLn $ replicate 60 '='
    putStrLn "EGYPTIAN MULTIPLICATION IN HASKELL"
    putStrLn "Functional Programming Meets Ancient Mathematics"
    putStrLn $ replicate 60 '='

    -- Basic demonstration
    putStrLn "\n=== Basic Algorithm ==="
    displayCalculation 13 17

    -- Edge cases
    putStrLn "\n=== Edge Cases ==="
    putStrLn $ "0 × 5 = " ++ show (egyptianMultiply 0 5)
    putStrLn $ "1 × 42 = " ++ show (egyptianMultiply 1 42)
    putStrLn $ "8 × 8 = " ++ show (egyptianMultiply 8 8) ++ " (power of 2)"
    putStrLn $ "32 × 1 = " ++ show (egyptianMultiply 32 1)

    -- Different implementations
    putStrLn "\n=== Different Functional Styles ==="
    let a = 25
        b = 11
    putStrLn $ "Testing " ++ show a ++ " × " ++ show b ++ ":"
    putStrLn $ "  Basic:       " ++ show (egyptianMultiply a b)
    putStrLn $ "  Recursive:   " ++ show (egyptianMultiplyRec a b)
    putStrLn $ "  Point-free:  " ++ show (egyptianMultiplyPointFree a b)
    putStrLn $ "  List comp:   " ++ show (egyptianMultiplyListComp a b)

    -- Extended with negatives
    putStrLn "\n=== Extended Algorithm (with negatives) ==="
    putStrLn $ "-13 × 17 = " ++ show (egyptianMultiplyExtended (-13) 17)
    putStrLn $ "13 × -17 = " ++ show (egyptianMultiplyExtended 13 (-17))
    putStrLn $ "-13 × -17 = " ++ show (egyptianMultiplyExtended (-13) (-17))

    -- Binary analysis
    analyzeBinaryConnection 13

    -- Historical examples
    historicalExamples

    -- Performance comparison
    benchmarkImplementations

    putStrLn "\n=== Mathematical and Functional Programming Insights ==="
    putStrLn $ unlines
        [ "The Egyptian multiplication algorithm beautifully demonstrates several"
        , "key functional programming concepts:"
        , ""
        , "1. RECURSION: The algorithm has a natural recursive structure that"
        , "   Haskell expresses elegantly."
        , ""
        , "2. UNFOLD: The algorithm is essentially an unfold operation, generating"
        , "   a sequence of (doubled, halved) pairs."
        , ""
        , "3. FILTER AND SUM: The final result comes from filtering odd positions"
        , "   and summing - a classic functional pattern."
        , ""
        , "4. MATHEMATICAL PROPERTIES: The algorithm satisfies mathematical laws"
        , "   (commutativity, identity, etc.) that we can verify with property testing."
        , ""
        , "5. HISTORICAL CONTINUITY: From ancient Egypt to modern Haskell, the"
        , "   mathematical insight remains unchanged - only our notation evolves."
        ]