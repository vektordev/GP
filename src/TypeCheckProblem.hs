module TypeCheckProblem(
  fitness
, generateInput
, worstScore
, bestScore
) where

--TODO: This file will generate Haskell expressions as Strings,
--give information about used functions, typeclasses etc and then
--expect the candidate to tell whether the given expression typechecks.
--it seems like a good idea to weight test cases so that non-type-checking
--tests have less of an impact.
--The function should be applicable to expression first and foremost,
--though it would be beneficial to also enable checking of whole functions
--the returned fitness value should establish a viable, learnable slope

--something along the lines of "echo \input\ | ghci"
--also, it will be useful to use "echo ":browse Prelude" | ghci"

--example I/O: In = ["map :: (a->b) -> [a] -> [b]", "(+ 1) :: Num a -> a -> a", "[1..3] :: (Enum t, Num t) => [t]]
--Out: map (+1)  [1..3] :: (Enum b, Num b) => [b]
--fitness: section in front of arrow is bonus, the closer the better. Low impact on fitness
--double-colon is required; highish impact.
--section after :: - every matching parameter adds points, every right contraint, correct result type; high total impact

--how to handle mismatches?

--how to phrase the problem of "does this type match this type"?

type Input = [String]
type Output = String

fitness :: Input -> Output -> IO Float
fitness = undefined

generateInput :: Int -> IO Input
generateInput = undefined
--general pattern: find all symbols. Pick n at random.
--expansion: use hoogle to find matching symbols maybe?

worstScore :: Input -> Float
worstScore x = 0.0

bestScore :: Input -> Float
bestScore x = 1.0
