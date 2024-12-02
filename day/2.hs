module Main where

main :: IO ()
main = do
    input <- lines <$> getContents
    let levels = map (map read . words) input :: [[Int]]
    print $ solve1 levels
    print $ solve2 levels

solve1 :: [[Int]] -> Int
solve1 levels = sum . map fromEnum . map isSafe $ levels

isSafe :: [Int] -> Bool
isSafe xs = isSafe' xs True || isSafe' xs False 

isSafe' :: [Int] -> Bool -> Bool
isSafe' [] _ = True
isSafe' [_] _ = True
isSafe' (x:y:rest) True = 0 < (y - x) && (y - x) <= 3  && isSafe' (y:rest) True
isSafe' (x:y:rest) False = -3 <= (y - x) && (y - x) < 0 && isSafe' (y:rest) False

solve2 :: [[Int]] -> Int
solve2 = length . filter canMakeSafe

canMakeSafe :: [Int] -> Bool
canMakeSafe xs = isSafe xs || any isSafe (removeOne xs)
  where
    removeOne [] = []
    removeOne (x:xs) = xs : map (x:) (removeOne xs)
